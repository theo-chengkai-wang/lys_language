open Lys_utils
open Lys_ast
open Core

module type TypingContext_type = sig
  module Key : sig
    type t [@@deriving sexp, equal, compare]

    val get_name : t -> string
    val of_string : string -> t
  end

  type 'b t [@@deriving sexp, equal, compare]

  val create_empty_context : unit -> 'b t
  val add_mapping : 'b t -> Key.t -> 'b -> 'b t

  (* val delete_last_mapping : 'b t -> Key.t -> 'b t *)
  val get_last_mapping : 'b t -> Key.t -> 'b option
  val add_all_mappings : 'b t -> (Key.t * 'b) list -> 'b t
  val is_in_context : 'b t -> Key.t -> bool
  val get_all_mappings_as_list : 'b t -> (Key.t * 'b) list
end

module TypingContext : functor (IdModule : Ast.Identifier_type) ->
  TypingContext_type with module Key = IdModule =
functor
  (IdModule : Ast.Identifier_type)
  ->
  struct
    module Inner = Context.NaiveContext.Make (String)
    module Key = IdModule

    type 'b t = 'b Inner.t [@@deriving sexp, equal, compare]

    let create_empty_context = Inner.create_empty_context
    let add_mapping ctx v = v |> Key.get_name |> Inner.add_mapping ctx
    let get_last_mapping ctx v = v |> Key.get_name |> Inner.get_last_mapping ctx

    let add_all_mappings ctx kvs =
      List.map ~f:(fun (k, v) -> (Key.get_name k, v)) kvs
      |> Inner.add_all_mappings ctx

    let is_in_context ctx k = k |> Key.get_name |> Inner.is_in_context ctx

    let get_all_mappings_as_list ctx =
      Inner.get_all_mappings_as_list ctx
      |> List.map ~f:(fun (k, v) -> (Key.of_string k, v))
  end

module ObjTypingContext :
  TypingContext_type with module Key = Ast.ObjIdentifier =
  TypingContext (Ast.ObjIdentifier)

module MetaTypingContext :
  TypingContext_type with module Key = Ast.MetaIdentifier =
  TypingContext (Ast.MetaIdentifier)

module PolyTypeVarContext : TypingContext_type with module Key = Ast.TypeVar =
  TypingContext (Ast.TypeVar)

module type TypeConstrContext_type = sig
  type constr_record = {
    type_params : Ast.TypeVarContext.t;
        (* Inefficient representation but acceptable*)
    constr : Ast.Constructor.t;
    arg_type : Ast.Typ.t option;
    belongs_to_typ : Ast.TypeIdentifier.t;
  }
  [@@deriving sexp, show, equal, compare]

  type t [@@deriving sexp, equal, compare]

  val add_typ_from_decl :
    t ->
    Ast.TypeVarContext.t
    * Ast.TypeIdentifier.t
    * (Ast.Constructor.t * Ast.Typ.t option) list ->
    t Or_error.t (*Error thrown when duplicated constructor name*)

  val get_constr_from_typ :
    t -> Ast.TypeIdentifier.t -> constr_record list option
  (*None means type doesn't exist, Some [] means type exists but is empty*)

  val get_typ_from_constr : t -> Ast.Constructor.t -> constr_record option
  val empty : t
  val typ_is_polymorphic : t -> Ast.TypeIdentifier.t -> bool option
  val constr_is_polymorphic : t -> Ast.Constructor.t -> bool option
  val show : t -> string
end

module ConstructorsMap = Map.Make (Ast.Constructor)
module TypMap = Map.Make (Ast.TypeIdentifier)

module TypeConstrContext : TypeConstrContext_type = struct
  type constr_record = {
    type_params : Ast.TypeVarContext.t;
        (* Inefficient representation but acceptable*)
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
      (tvctx, tid, constructor_type_list) =
    match
      List.find constructor_type_list ~f:(fun (constr, _) ->
          ConstructorsMap.mem constr_typ_map constr)
    with
    | Some (c, _) ->
        Or_error.error
          (Printf.sprintf
             "TypeConstrTypingContextError: Constructor %s already defined"
             (Ast.Constructor.get_name c))
          (c, tid, constructor_type_list)
          [%sexp_of:
            Ast.Constructor.t
            * Ast.TypeIdentifier.t
            * (Ast.Constructor.t * Ast.Typ.t option) list]
    | None ->
        let new_records =
          List.map constructor_type_list ~f:(fun (constr, typ) ->
              {
                type_params = tvctx;
                constr;
                arg_type = typ;
                belongs_to_typ = tid;
              })
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

  let typ_is_polymorphic ctx tid =
    let open Option.Monad_infix in
    get_constr_from_typ ctx tid >>= function
    | [] -> Some false
    | { type_params; _ } :: _ -> Some (Ast.TypeVarContext.is_empty type_params)

  let constr_is_polymorphic ctx constr =
    let open Option.Monad_infix in
    get_typ_from_constr ctx constr >>= fun { type_params; _ } ->
    Some (Ast.TypeVarContext.is_empty type_params)

  let to_typing_decl ctx =
    let fold_func ~key ~data acc =
      match List.hd data with
      | None -> []
      | Some { type_params; _ } ->
          type_params |> fun type_params ->
          ( type_params,
            key,
            List.map data ~f:(fun record -> (record.constr, record.arg_type)) )
          :: acc
    in
    TypMap.fold ctx.typ_constr_map ~init:[] ~f:fold_func

  let show ctx =
    ctx |> to_typing_decl
    |> List.fold ~init:"" ~f:(fun acc (type_params, tid, constr_typ_list) ->
           acc
           ^ Printf.sprintf "\t (%s) %s : \n\t\t%s;\n"
               (List.fold ~init:""
                  ~f:(fun acc tv -> acc ^ "'" ^ Ast.TypeVar.get_name tv ^ ",")
                  type_params)
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
