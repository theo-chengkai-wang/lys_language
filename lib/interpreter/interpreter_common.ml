open Lys_ast
open Core
open Lys_utils
open Lys_typing

(* Top level evaluation context *)
module EvaluationContext : sig
  type single_record = { typ : Ast.Typ.t; is_rec : bool; value : Ast.Value.t }
  [@@deriving show, sexp, compare, equal]

  type t = single_record String_map.t [@@deriving sexp, compare, equal]

  val set : t -> key:string -> data:single_record -> t
  val find_or_error : t -> string -> single_record Or_error.t
  val empty : t
  val show : t -> string
  val to_typing_obj_context : t -> Ast.Typ.t Typing_context.ObjTypingContext.t
end = struct
  type single_record = { typ : Ast.Typ.t; is_rec : bool; value : Ast.Value.t }
  [@@deriving show, sexp, compare, equal]

  type t = single_record String_map.t [@@deriving sexp, compare, equal]

  let set = String_map.set

  let find_or_error map key =
    match String_map.find map key with
    | Some v -> Ok v
    | None ->
        error "EvaluationContextError: Key not found." key [%sexp_of: string]

  let empty = String_map.empty

  let show v =
    v |> String_map.to_alist
    |> List.fold ~init:"" ~f:(fun acc (id, record) ->
           acc ^ Printf.sprintf "(%s, %s);" id (show_single_record record))
    |> fun str -> Printf.sprintf "[\n%s]" str

  let to_typing_obj_context v =
    v |> String_map.to_alist
    |> List.map ~f:(fun (id, record) ->
           (Ast.ObjIdentifier.of_string id, record.typ))
    |> Typing_context.ObjTypingContext.add_all_mappings
         (Typing_context.ObjTypingContext.create_empty_context ())
end

module type TypeConstrContext_type = sig
  type constr_record = {
    constr : Ast.Constructor.t;
    arg_type : Ast.Typ.t option;
    belongs_to_typ : Ast.TypeIdentifier.t;
  }
  [@@deriving sexp, show, equal, compare]

  type t [@@deriving sexp, equal, compare]

  val add_typ_from_decl :
    t ->
    Ast.TypeIdentifier.t * (Ast.Constructor.t * Ast.Typ.t option) list ->
    t Or_error.t (*Error thrown when duplicated constructor name*)

  val get_constr_from_typ :
    t -> Ast.TypeIdentifier.t -> constr_record list option
  (*None means type doesn't exist, Some [] means type exists but is empty*)

  val get_typ_from_constr : t -> Ast.Constructor.t -> constr_record option
  val empty : t

  val to_typing_decl :
    t ->
    (Ast.TypeIdentifier.t * (Ast.Constructor.t * Ast.Typ.t option) list) list

  val to_typeconstrtypingcontext : t -> Typing_context.TypeConstrTypingContext.t
  val show : t -> string
end

module ConstructorsMap = Map.Make (Ast.Constructor)
module TypMap = Map.Make (Ast.TypeIdentifier)

module TypeConstrContext : TypeConstrContext_type = struct
  type constr_record = {
    constr : Ast.Constructor.t;
    arg_type : Ast.Typ.t option;
    belongs_to_typ : Ast.TypeIdentifier.t;
  }
  [@@deriving sexp, show, equal, compare]

  type t = {
    typ_constr_map : constr_record list TypMap.t;
    constr_typ_map : constr_record ConstructorsMap.t;
  }
  [@@deriving sexp, equal, compare]

  let add_typ_from_decl { typ_constr_map; constr_typ_map }
      (tid, constructor_type_list) =
    match
      List.find constructor_type_list ~f:(fun (constr, _) ->
          ConstructorsMap.mem constr_typ_map constr)
    with
    | Some (c, _) ->
        Or_error.error
          (Printf.sprintf
             "TypeConstrContextError: Constructor %s already defined"
             (Ast.Constructor.get_name c))
          (c, tid, constructor_type_list)
          [%sexp_of:
            Ast.Constructor.t
            * Ast.TypeIdentifier.t
            * (Ast.Constructor.t * Ast.Typ.t option) list]
    | None ->
        let new_records =
          List.map constructor_type_list ~f:(fun (constr, typ) ->
              { constr; arg_type = typ; belongs_to_typ = tid })
        in
        let new_typ_constr_map =
          TypMap.set typ_constr_map ~key:tid ~data:new_records
        in
        let new_constr_typ_map =
          List.fold new_records ~init:constr_typ_map ~f:(fun acc n_record ->
              ConstructorsMap.set acc ~key:n_record.constr ~data:n_record)
        in
        Ok
          {
            typ_constr_map = new_typ_constr_map;
            constr_typ_map = new_constr_typ_map;
          }

  let get_constr_from_typ { typ_constr_map; _ } typ =
    TypMap.find typ_constr_map typ

  let get_typ_from_constr { constr_typ_map; _ } constr =
    ConstructorsMap.find constr_typ_map constr

  let empty =
    { typ_constr_map = TypMap.empty; constr_typ_map = ConstructorsMap.empty }

  let to_typing_decl ctx =
    let fold_func ~key ~data acc =
      (key, List.map data ~f:(fun record -> (record.constr, record.arg_type)))
      :: acc
    in
    TypMap.fold ctx.typ_constr_map ~init:[] ~f:fold_func

  let to_typeconstrtypingcontext ctx =
    (*Use ok_exn here because no error is expected*)
    to_typing_decl ctx
    |> List.fold ~init:Typing_context.TypeConstrTypingContext.empty
         ~f:(fun acc x ->
           Typing_context.TypeConstrTypingContext.add_typ_from_decl acc x
           |> ok_exn)

  let show ctx =
    ctx |> to_typing_decl
    |> List.fold ~init:"" ~f:(fun acc (tid, constr_typ_list) ->
           acc
           ^ Printf.sprintf "\t %s : \n\t\t%s;\n"
               (Ast.TypeIdentifier.show tid)
               ("["
               ^ List.fold constr_typ_list ~init:""
                   ~f:(fun acc (constr, typ_opt) ->
                     acc
                     ^ Printf.sprintf "(%s, %s);"
                         (Ast.Constructor.get_name constr)
                         (match typ_opt with
                         | None -> "None"
                         | Some typ ->
                             Printf.sprintf "Some (%s)" (Ast.Typ.show typ)))
               ^ "]"))
    |> Printf.sprintf "[\n%s]"
end

module TopLevelEvaluationResult = struct
  type t =
    | ExprValue of Ast.Typ.t * Ast.Value.t * float option * int option
    | Defn of Ast.IdentifierDefn.t * Ast.Value.t * float option * int option
    | RecDefn of Ast.IdentifierDefn.t * Ast.Value.t * float option * int option
    | Directive of Ast.Directive.t * string
    | DatatypeDecl of
        Ast.TypeIdentifier.t * (Ast.Constructor.t * Ast.Typ.t option) list
  [@@deriving sexp, compare, equal, show]

  let get_str_output res =
    Printf.sprintf "------------------------------\n"
    ^
    match res with
    | ExprValue (typ, v, time_elapsed_opt, reduction_steps_opt) ->
        let time_preface =
          match time_elapsed_opt with
          | None -> ""
          | Some time -> Printf.sprintf "Time elapsed (ns): %f\n" time
        in
        let reduction_steps_preface =
          match reduction_steps_opt with
          | None -> ""
          | Some steps -> Printf.sprintf "Reduction steps (#): %d\n" steps
        in
        Printf.sprintf "%s%sval:\n\t%s \n=\n\t %s" time_preface
          reduction_steps_preface (Ast.Typ.show typ) (Ast.Value.show v)
    | Defn ((id, typ), v, time_elapsed_opt, reduction_steps_opt) ->
        let time_preface =
          match time_elapsed_opt with
          | None -> ""
          | Some time -> Printf.sprintf "Time elapsed (ns): %f\n" time
        in
        let reduction_steps_preface =
          match reduction_steps_opt with
          | None -> ""
          | Some steps -> Printf.sprintf "Reduction steps (#): %d\n" steps
        in
        Printf.sprintf "%s%sval %s :\n\t%s \n=\n\t %s" time_preface
          reduction_steps_preface
          (Ast.ObjIdentifier.get_name id)
          (Ast.Typ.show typ) (Ast.Value.show v)
    | RecDefn ((id, typ), v, time_elapsed_opt, reduction_steps_opt) ->
        let time_preface =
          match time_elapsed_opt with
          | None -> ""
          | Some time -> Printf.sprintf "Time elapsed (ns): %f\n" time
        in
        let reduction_steps_preface =
          match reduction_steps_opt with
          | None -> ""
          | Some steps -> Printf.sprintf "Reduction steps (#): %d\n" steps
        in
        Printf.sprintf "%s%sval rec %s:\n\t %s \n=\n\t %s" time_preface
          reduction_steps_preface
          (Ast.ObjIdentifier.get_name id)
          (Ast.Typ.show typ) (Ast.Value.show v)
    | Directive (d, message) ->
        Printf.sprintf
          "Executed directive %s\n\tMessage from the directive:\n\t %s"
          (Ast.Directive.show d) message
    | DatatypeDecl (tid, constr_typ_list) ->
        let init =
          Printf.sprintf "datatype %s = \n" (Ast.TypeIdentifier.get_name tid)
        in
        List.fold constr_typ_list ~init ~f:(fun acc (constr, typ_opt) ->
            acc
            ^
            match typ_opt with
            | None ->
                Printf.sprintf "\t| %s\n" (Ast.Constructor.get_name constr)
            | Some typ ->
                Printf.sprintf "\t| %s of\n\t\t%s\n"
                  (Ast.Constructor.get_name constr)
                  (Ast.Typ.show typ))
end
