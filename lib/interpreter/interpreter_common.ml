open Lys_ast
open Core
open Lys_typing

(* Top level evaluation context *)
module EvaluationContext : sig
  type single_record = {
    typ : Ast.Typ.t;
    rec_preface : (Ast.IdentifierDefn.t * Ast.Expr.t) list;
    value : Ast.Value.t;
  }
  [@@deriving show, sexp, compare, equal]

  type t = single_record String.Map.t [@@deriving sexp, compare, equal]

  val set : t -> key:string -> data:single_record -> t
  val set_all : t -> (string * single_record) list -> t
  val find_or_error : t -> string -> single_record Or_error.t
  val empty : t
  val show : t -> string

  val to_typing_obj_context :
    t -> (Ast.Typ.t * int) Typing_context.ObjTypingContext.t

  val is_not_rec : single_record -> bool
  val is_single_rec : single_record -> bool
  val is_mut_rec : single_record -> bool
end = struct
  type single_record = {
    typ : Ast.Typ.t;
    rec_preface : (Ast.IdentifierDefn.t * Ast.Expr.t) list;
    value : Ast.Value.t;
  }
  [@@deriving show, sexp, compare, equal]

  type t = single_record String.Map.t [@@deriving sexp, compare, equal]

  let set = String.Map.set

  let set_all m kvs =
    List.fold kvs ~init:m ~f:(fun acc (key, data) ->
        String.Map.set acc ~key ~data)

  let find_or_error map key =
    match String.Map.find map key with
    | Some v -> Ok v
    | None ->
        error "EvaluationContextError: Key not found." key [%sexp_of: string]

  let empty = String.Map.empty

  let show v =
    v |> String.Map.to_alist
    |> List.fold ~init:"" ~f:(fun acc (id, record) ->
           acc ^ Printf.sprintf "(%s, %s);" id (show_single_record record))
    |> fun str -> Printf.sprintf "[\n%s]" str

  let to_typing_obj_context v =
    v |> String.Map.to_alist
    |> List.map ~f:(fun (id, record) ->
           (Ast.ObjIdentifier.of_string id, (record.typ, 0)))
    |> Typing_context.ObjTypingContext.add_all_mappings
         (Typing_context.ObjTypingContext.create_empty_context ())

  let is_not_rec s = List.is_empty s.rec_preface
  let is_single_rec s = 1 = List.length s.rec_preface
  let is_mut_rec s = 1 < List.length s.rec_preface
end

module TypeConstrContext = Typing_context.TypeConstrContext
(* Kept here for legacy reasons *)

module TopLevelEvaluationResult = struct
  type verbose = { steps : Ast.Expr.t list }
  [@@deriving sexp, compare, equal, show]

  type t =
    | ExprValue of
        Ast.Typ.t * Ast.Value.t * float option * int option * verbose option
    | Defn of
        Ast.IdentifierDefn.t
        * Ast.Value.t
        * float option
        * int option
        * verbose option
    | RecDefn of
        Ast.IdentifierDefn.t
        * Ast.Value.t
        * float option
        * int option
        * verbose option
    | MutRecDefn of
        (Ast.IdentifierDefn.t
        * Ast.Value.t
        * float option
        * int option
        * verbose option)
        list
    | Directive of Ast.Directive.t * string
    | DatatypeDecl of
        (Ast.TypeVarContext.t
        * Ast.TypeIdentifier.t
        * (Ast.Constructor.t * Ast.Typ.t option) list)
        list
  [@@deriving sexp, compare, equal, show]

  let get_str_output ?(pretty_print = false) res =
    let show_typ, show_val, show_verbose =
      if pretty_print then
        ( Ast.Typ.pretty_print,
          Ast.Value.pretty_print ~alinea_size:1,
          fun { steps } ->
            Lys_utils.Utils.list_pretty_print ~sep:"\n=====>>>>>\n"
              ~pretty_print:(Ast.Expr.pretty_print ~alinea_size:0)
              steps )
      else (Ast.Typ.show, Ast.Value.show, show_verbose)
    in
    Printf.sprintf "------------------------------\n"
    ^
    match res with
    | ExprValue (typ, v, time_elapsed_opt, reduction_steps_opt, verbose_opt) ->
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
        let reduction_steps_postface =
          match verbose_opt with
          | None -> ""
          | Some v ->
              "------------------STEPS----------------\n" ^ show_verbose v
              ^ "\n"
        in

        Printf.sprintf "%s%sval:\n\t%s \n=\n\t %s\n%s" time_preface
          reduction_steps_preface (show_typ typ) (show_val v)
          reduction_steps_postface
    | Defn ((id, typ), v, time_elapsed_opt, reduction_steps_opt, verbose_opt) ->
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
        let reduction_steps_postface =
          match verbose_opt with
          | None -> ""
          | Some v ->
              "------------------STEPS----------------\n" ^ show_verbose v
              ^ "\n"
        in
        Printf.sprintf "%s%sval %s :\n\t%s \n=\n\t %s\n%s" time_preface
          reduction_steps_preface
          (Ast.ObjIdentifier.get_name id)
          (show_typ typ) (show_val v) reduction_steps_postface
    | RecDefn ((id, typ), v, time_elapsed_opt, reduction_steps_opt, verbose_opt)
      ->
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
        let reduction_steps_postface =
          match verbose_opt with
          | None -> ""
          | Some v ->
              "------------------STEPS----------------\n" ^ show_verbose v
              ^ "\n"
        in
        Printf.sprintf "%s%sval rec %s:\n\t %s \n=\n\t %s\n%s" time_preface
          reduction_steps_preface
          (Ast.ObjIdentifier.get_name id)
          (show_typ typ) (show_val v) reduction_steps_postface
    | MutRecDefn iddef_v_time_red_verbose_list ->
        let process_1
            ((id, typ), v, time_elapsed_opt, reduction_steps_opt, verbose_opt) =
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
          let reduction_steps_postface =
            match verbose_opt with
            | None -> ""
            | Some v ->
                "------------------STEPS----------------\n" ^ show_verbose v
                ^ "\n"
          in
          Printf.sprintf "%s%sval rec %s:\n\t %s \n=\n\t %s\n%s" time_preface
            reduction_steps_preface
            (Ast.ObjIdentifier.get_name id)
            (show_typ typ) (show_val v) reduction_steps_postface
        in
        let processed = List.map ~f:process_1 iddef_v_time_red_verbose_list in
        String.concat ~sep:"-------------------------\nand\n" processed
    | Directive (d, message) ->
        Printf.sprintf
          "Executed directive %s\n\tMessage from the directive:\n\t %s"
          (Ast.Directive.show d) message
    | DatatypeDecl id_constr_typ_list_list ->
        let process_single_decl (tvctx, tid, constr_typ_list) =
          (* Note that here I use _exn functions because no error is expected ever. *)
          let tvctx_printed =
            if not (Ast.TypeVarContext.is_empty tvctx) then
              Printf.sprintf " (%s)"
                (List.fold
                   ~init:
                     (Printf.sprintf "'%s"
                        (Ast.TypeVar.get_name (List.hd_exn tvctx)))
                   ~f:(fun acc tv ->
                     Printf.sprintf "%s, '%s" acc (Ast.TypeVar.get_name tv))
                   (List.tl_exn tvctx))
            else ""
          in
          let init =
            Printf.sprintf "datatype%s %s = \n" tvctx_printed
              (Ast.TypeIdentifier.get_name tid)
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
                    (show_typ typ))
        in
        List.fold id_constr_typ_list_list ~init:""
          ~f:(fun acc (tvctx, tid, constr_typ_list) ->
            let single_str =
              process_single_decl (tvctx, tid, constr_typ_list)
            in
            acc ^ single_str)
end
