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

(*
   let reduce ~top_level_context ~expr = ()

   let evaluate_program program = [] *)

let rec multi_step_reduce ~top_level_context ~type_constr_context ~expr =
  let open Or_error.Monad_infix in
  match expr with
  | Ast.Expr.Identifier id ->
      (*No error if we're dealing with sth at top level context, otherwise error*)
      let index = Ast.ObjIdentifier.get_debruijn_index id in
      if not (Ast.DeBruijnIndex.equal index Ast.DeBruijnIndex.top_level) then
        (*Error*)
        error
          "EvaluationError: Can only evaluate a top level identifier. A non \
           top-level identifier \n\
          \    must have been substituted already." id
          [%sexp_of: Ast.ObjIdentifier.t]
      else
        let id_str = Ast.ObjIdentifier.get_name id in
        EvaluationContext.find_or_error top_level_context id_str
        >>= fun { typ; is_rec; value } ->
        if not is_rec then
          (*Substitution -- no need to worry about De Bruijn indices as there is no way they can go wrong*)
          Ok value
        else
          (* Recursion: handle once together. *)
          Ast.DeBruijnIndex.create 0 >>= fun debruijn_index ->
          multi_step_reduce ~top_level_context ~type_constr_context
            ~expr:
              (Ast.Expr.LetRec
                 ( (Ast.ObjIdentifier.of_string id_str, typ),
                   Ast.Value.to_expr value,
                   Ast.Expr.Identifier
                     (Ast.ObjIdentifier.of_string_and_index id_str
                        debruijn_index) ))
  | Ast.Expr.Constant c -> Ok (Ast.Value.Constant c)
  | Ast.Expr.UnaryOp (op, expr) -> (
      multi_step_reduce ~top_level_context ~type_constr_context ~expr
      >>= fun v ->
      match (op, v) with
      | Ast.UnaryOperator.NEG, Ast.Value.Constant (Ast.Constant.Integer i) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (-i)))
      | Ast.UnaryOperator.NOT, Ast.Value.Constant (Ast.Constant.Boolean b) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (not b)))
      | _, _ ->
          error
            "EvaluationError: type mismatch at Unary operator. [FATAL] should \
             not happen! Type check should have prevented this."
            (Ast.Expr.UnaryOp (op, expr))
            [%sexp_of: Ast.Expr.t])
  | Ast.Expr.BinaryOp (op, expr1, expr2) -> (
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:expr1
      >>= fun v1 ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:expr2
      >>= fun v2 ->
      match (op, v1, v2) with
      | ( Ast.BinaryOperator.ADD,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 + i2)))
      | ( Ast.BinaryOperator.SUB,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 - i2)))
      | ( Ast.BinaryOperator.MUL,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 * i2)))
      | ( Ast.BinaryOperator.DIV,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 / i2)))
      | ( Ast.BinaryOperator.MOD,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 % i2)))
      | Ast.BinaryOperator.EQ, v1, v2 ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (Ast.Value.equal v1 v2)))
      | Ast.BinaryOperator.NEQ, v1, v2 ->
          Ok
            (Ast.Value.Constant
               (Ast.Constant.Boolean (not (Ast.Value.equal v1 v2))))
      | ( Ast.BinaryOperator.GTE,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 >= i2)))
      | ( Ast.BinaryOperator.GT,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 > i2)))
      | ( Ast.BinaryOperator.LT,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 < i2)))
      | ( Ast.BinaryOperator.LTE,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 <= i2)))
      | ( Ast.BinaryOperator.AND,
          Ast.Value.Constant (Ast.Constant.Boolean i1),
          Ast.Value.Constant (Ast.Constant.Boolean i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 && i2)))
      | ( Ast.BinaryOperator.OR,
          Ast.Value.Constant (Ast.Constant.Boolean i1),
          Ast.Value.Constant (Ast.Constant.Boolean i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 || i2)))
      | _, _, _ ->
          error
            "EvaluationError: type mismatch at Binary operator. [FATAL] should \
             not happen! Type check should have prevented this."
            (Ast.Expr.BinaryOp (op, expr1, expr2))
            [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Prod exprs ->
      List.map exprs ~f:(fun expr ->
          multi_step_reduce ~top_level_context ~type_constr_context ~expr)
      |> Or_error.combine_errors
      >>= fun values -> Ok (Ast.Value.Prod values)
  (* | Ast.Expr.Fst expr -> (
         multi_step_reduce ~top_level_context ~type_constr_context ~expr >>= fun v ->
         match v with
         | Ast.Value.Prod (v1, _) -> Ok v1
         | _ ->
             error
               "EvaluationError: type mismatch at Fst. [FATAL] should not happen! \
                Type check should have prevented this."
               (Ast.Expr.Fst expr) [%sexp_of: Ast.Expr.t])
     | Ast.Expr.Snd expr -> (
         multi_step_reduce ~top_level_context ~type_constr_context ~expr >>= fun v ->
         match v with
         | Ast.Value.Prod (_, v2) -> Ok v2
         | _ ->
             error
               "EvaluationError: type mismatch at Snd. [FATAL] should not happen! \
                Type check should have prevented this."
               (Ast.Expr.Snd expr) [%sexp_of: Ast.Expr.t]) *)
  | Ast.Expr.Nth (expr, i) -> (
      multi_step_reduce ~top_level_context ~type_constr_context ~expr
      >>= fun v ->
      match v with
      | Ast.Value.Prod values -> (
          match List.nth values i with
          | None ->
              error
                "EvaluationError: type mismatch at Nth: access out of bounds. \
                 [FATAL] should not happen! Type check should have prevented \
                 this."
                (Ast.Expr.Nth (expr, i))
                [%sexp_of: Ast.Expr.t]
          | Some v -> Ok v)
      | _ ->
          error
            "EvaluationError: type mismatch at Nth. [FATAL] should not happen! \
             Type check should have prevented this."
            (Ast.Expr.Nth (expr, i))
            [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Left (t1, t2, expr) ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr
      >>= fun v -> Ok (Ast.Value.Left (t1, t2, v))
      (* We don't run RT type check here. *)
  | Ast.Expr.Right (t1, t2, expr) ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr
      >>= fun v -> Ok (Ast.Value.Right (t1, t2, v))
      (* We don't run RT type check here. *)
  | Ast.Expr.Case (e, (id1, _), e1, (id2, _), e2) ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun v ->
      let new_expr_or_error =
        match v with
        | Ast.Value.Left (_, _, v) ->
            Substitutions.substitute (Ast.Value.to_expr v) id1 e1
        | Ast.Value.Right (_, _, v) ->
            Substitutions.substitute (Ast.Value.to_expr v) id2 e2
        | _ ->
            error
              "EvaluationError: type mismatch at match clause. [FATAL] should \
               not happen! Type check should have prevented this."
              expr [%sexp_of: Ast.Expr.t]
      in
      new_expr_or_error >>= fun new_expr ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:new_expr
  | Ast.Expr.Lambda (iddef, e) -> Ok (Ast.Value.Lambda (iddef, e))
  | Ast.Expr.Application (e1, e2) -> (
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e1
      >>= fun v1 ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e2
      >>= fun v2 ->
      match v1 with
      | Ast.Value.Lambda ((id, _), e) ->
          Substitutions.substitute (Ast.Value.to_expr v2) id e
          >>= fun new_expr ->
          multi_step_reduce ~top_level_context ~type_constr_context
            ~expr:new_expr
      | _ ->
          error
            "EvaluationError: type mismatch at Lambda. [FATAL] should not \
             happen! Type check should have prevented this."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.IfThenElse (b, e1, e2) -> (
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:b
      >>= fun bv ->
      match bv with
      | Ast.Value.Constant (Ast.Constant.Boolean v) ->
          if v then
            multi_step_reduce ~top_level_context ~type_constr_context ~expr:e1
          else
            multi_step_reduce ~top_level_context ~type_constr_context ~expr:e2
      | _ ->
          error
            "EvaluationError: type mismatch at IfThenElse. [FATAL] should not \
             happen! Type check should have prevented this."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.LetBinding ((id, _), e, e2) ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun ev ->
      Substitutions.substitute (Ast.Value.to_expr ev) id e2 >>= fun e ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
  | Ast.Expr.LetRec ((id, typ), e, e2) ->
      (* let rec f = v in e  ~~~~~> [([(let rec f = v in f)/f]v)/f]e

         Analysis
           let rec f = (0)
               v (1 -- means that index 0 at top level = f)
           in
               e (1)

           (fun f (0)-> e(1)) ((fun f (0) -> v(1)) (let rec f(0) = v(1) in f(1)))

         So Shifting is not really needed!
         However, I can't just translate it to the above, for it will easily stack overflow, in a very unfortunate way,
         do to my call by value semantics. Instead, I shall have to actually manually write the substitutions out.
      *)
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun v ->
      Ast.DeBruijnIndex.create 0 >>= fun db_index_0 ->
      (*let v1 = [(let rec f = v in f)/f]v*)
      let ev = Ast.Value.to_expr v in
      let to_sub_in =
        Ast.Expr.LetRec
          ( (id, typ),
            ev,
            Ast.Expr.Identifier
              (Ast.ObjIdentifier.of_string_and_index
                 (Ast.ObjIdentifier.get_name id)
                 db_index_0) )
      in
      Substitutions.substitute to_sub_in id ev >>= fun ev1 ->
      (*[v1/f]e*)
      Substitutions.substitute ev1 id e2 >>= fun ev2 ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:ev2
  | Ast.Expr.Box (ctx, e) -> Ok (Ast.Value.Box (ctx, e))
  | Ast.Expr.LetBox (metaid, e, e2) -> (
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun box_v ->
      match box_v with
      | Ast.Value.Box (ctx, e_box) ->
          Substitutions.meta_substitute ctx e_box metaid e2 |> fun or_error ->
          Or_error.tag_arg or_error
            "EvaluationError: Meta substitution error: metaid, e->v, ctx, v"
            (metaid, box_v, ctx, e2)
            [%sexp_of:
              Ast.MetaIdentifier.t * Ast.Value.t * Ast.Context.t * Ast.Expr.t]
          >>= fun res_meta_sub ->
          multi_step_reduce ~top_level_context ~type_constr_context
            ~expr:res_meta_sub
      | _ ->
          error
            "EvaluationError: type mismatch at LetBox. [FATAL] should not \
             happen! Type check should have prevented this."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Closure (_, _) ->
      error "EvaluationError: One should never have to evaluate a raw closure."
        expr [%sexp_of: Ast.Expr.t]
  | Ast.Expr.Lift (_, expr) ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr
      >>= fun v ->
      let expr_v = Ast.Value.to_expr v in
      Ok (Ast.Value.Box ([], expr_v))
  | Ast.Expr.Constr (constr, e_opt) -> (
      if
        Option.is_none
          (TypeConstrContext.get_typ_from_constr type_constr_context constr)
      then
        error "EvaluationError: FATAL: constructor undefined" constr
          [%sexp_of: Ast.Constructor.t]
      else
        match e_opt with
        | None -> Ok (Ast.Value.Constr (constr, None))
        | Some e ->
            multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
            >>= fun v -> Ok (Ast.Value.Constr (constr, Some v)))
  | Ast.Expr.Match (e, pattn_expr_list) -> (
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun v ->
      let match_pattn_expr v (pattn, expr) =
        (* Processes the pattern: if not matched, return None; if matched return Some (Value.t Or_error.t) *)
        match (pattn, v) with
        | Ast.Pattern.Wildcard, _ ->
            multi_step_reduce ~top_level_context ~type_constr_context ~expr
            |> Some
        | Ast.Pattern.Id id, _ ->
            let expr_of_v = Ast.Value.to_expr v in
            Substitutions.substitute expr_of_v id expr
            >>= (fun substituted_expr ->
                  multi_step_reduce ~top_level_context ~type_constr_context
                    ~expr:substituted_expr)
            |> Some
        | Ast.Pattern.Inl id, Ast.Value.Left (_, _, v) ->
            let expr_of_v = Ast.Value.to_expr v in
            Substitutions.substitute expr_of_v id expr
            >>= (fun substituted_expr ->
                  multi_step_reduce ~top_level_context ~type_constr_context
                    ~expr:substituted_expr)
            |> Some
        | Ast.Pattern.Inr id, Ast.Value.Right (_, _, v) ->
            let expr_of_v = Ast.Value.to_expr v in
            Substitutions.substitute expr_of_v id expr
            >>= (fun substituted_expr ->
                  multi_step_reduce ~top_level_context ~type_constr_context
                    ~expr:substituted_expr)
            |> Some
        | Ast.Pattern.Prod id_list, Ast.Value.Prod value_list ->
            let expr_list = List.map value_list ~f:Ast.Value.to_expr in
            let idstr_list = List.map id_list ~f:Ast.ObjIdentifier.get_name in
            Utils.try_zip_list_or_error expr_list idstr_list
              (Or_error.error
                 "EvaluationError: argument number mismatch at match clause."
                 (pattn, expr) [%sexp_of: Ast.Pattern.t * Ast.Expr.t])
            >>= (fun zipped_list ->
                  Substitutions.sim_substitute_from_zipped_list zipped_list expr
                  >>= fun substituted_expr ->
                  multi_step_reduce ~top_level_context ~type_constr_context
                    ~expr:substituted_expr)
            |> Some
        | Ast.Pattern.Datatype (constr, []), Ast.Value.Constr (constr2, None) ->
            (* CHECK IF constr exists *)
            if
              Option.is_none
                (TypeConstrContext.get_typ_from_constr type_constr_context
                   constr)
            then
              error "EvaluationError: FATAL: constructor undefined" constr
                [%sexp_of: Ast.Constructor.t]
              |> Some
            else if not (Ast.Constructor.equal constr constr2) then None
            else
              multi_step_reduce ~top_level_context ~type_constr_context ~expr
              |> Some
        | ( Ast.Pattern.Datatype (constr, [ id ]),
            Ast.Value.Constr (constr2, Some v) ) ->
            if
              Option.is_none
                (TypeConstrContext.get_typ_from_constr type_constr_context
                   constr)
            then
              error "EvaluationError: FATAL: constructor undefined" constr
                [%sexp_of: Ast.Constructor.t]
              |> Some
            else if not (Ast.Constructor.equal constr constr2) then None
            else
              (* If constructors match, no of arguments should match too, otherwise error *)
              let expr_to_sub_for = Ast.Value.to_expr v in
              Substitutions.substitute expr_to_sub_for id expr
              >>= (fun substituted_expr ->
                    multi_step_reduce ~top_level_context ~type_constr_context
                      ~expr:substituted_expr)
              |> Some
        | ( Ast.Pattern.Datatype (constr, id_list),
            Ast.Value.Constr (constr2, Some (Ast.Value.Prod vlist)) ) ->
            if
              Option.is_none
                (TypeConstrContext.get_typ_from_constr type_constr_context
                   constr)
            then
              error "EvaluationError: FATAL: constructor undefined" constr
                [%sexp_of: Ast.Constructor.t]
              |> Some
            else if not (Ast.Constructor.equal constr constr2) then None
            else
              let expr_list = List.map vlist ~f:Ast.Value.to_expr in
              let idstr_list = List.map id_list ~f:Ast.ObjIdentifier.get_name in
              Utils.try_zip_list_or_error expr_list idstr_list
                (Or_error.error
                   "EvaluationError: argument number mismatch at match clause."
                   (pattn, expr) [%sexp_of: Ast.Pattern.t * Ast.Expr.t])
              >>= (fun zipped_list ->
                    Substitutions.sim_substitute_from_zipped_list zipped_list
                      expr
                    >>= fun substituted_expr ->
                    multi_step_reduce ~top_level_context ~type_constr_context
                      ~expr:substituted_expr)
              |> Some
        | _ -> None
      in
      let match_and_exec_result =
        Utils.list_traverse_and_try pattn_expr_list ~f:(fun (pattn, expr) ->
            match_pattn_expr v (pattn, expr))
      in
      match match_and_exec_result with
      | None ->
          error "EvaluationError: Pattern matching non-exhaustive."
            (Ast.Expr.Match (e, pattn_expr_list))
            [%sexp_of: Ast.Expr.t]
      | Some res -> res)
(*
   | Ast.Expr.Identifier id -> Ok ()
   | Ast.Expr.Constant c -> Ok ()
   | Ast.Expr.UnaryOp (op, expr) -> Ok ()
   | Ast.Expr.BinaryOp (op, expr, expr2) -> Ok ()
   | Ast.Expr.Prod (expr1, expr2) -> Ok ()
   | Ast.Expr.Fst expr -> Ok ()
   | Ast.Expr.Snd expr -> Ok ()
   | Ast.Expr.Left (t1, t2, expr) -> Ok ()
   | Ast.Expr.Right (t1, t2, expr) -> Ok ()
   | Ast.Expr.Match (e, iddef1, e1, iddef2, e2) -> Ok ()
   | Ast.Expr.Lambda (iddef, e) -> Ok ()
   | Ast.Expr.Application (e1, e2) -> Ok ()
   | Ast.Expr.IfThenElse (b, e1, e2) -> Ok ()
   | Ast.Expr.LetBinding (iddef, e, e2) -> Ok ()
   | Ast.Expr.LetRec (iddef, e, e2) -> Ok ()
   | Ast.Expr.Box (ctx, e) -> Ok ()
   | Ast.Expr.LetBox (metaid, e, e2) -> Ok ()
   | Ast.Expr.Closure (metaid, exprs) -> Ok ()
*)

module TopLevelEvaluationResult = struct
  type t =
    | ExprValue of Ast.Typ.t * Ast.Value.t * float option
    | Defn of Ast.IdentifierDefn.t * Ast.Value.t * float option
    | RecDefn of Ast.IdentifierDefn.t * Ast.Value.t * float option
    | Directive of Ast.Directive.t * string
    | DatatypeDecl of
        Ast.TypeIdentifier.t * (Ast.Constructor.t * Ast.Typ.t option) list
  [@@deriving sexp, compare, equal, show]

  let get_str_output res =
    Printf.sprintf "------------------------------\n"
    ^
    match res with
    | ExprValue (typ, v, time_elapsed_opt) ->
        let time_preface =
          match time_elapsed_opt with
          | None -> ""
          | Some time -> Printf.sprintf "Time elapsed (ns): %f\n" time
        in
        Printf.sprintf "%sval:\n\t%s \n=\n\t %s" time_preface (Ast.Typ.show typ)
          (Ast.Value.show v)
    | Defn ((id, typ), v, time_elapsed_opt) ->
        let time_preface =
          match time_elapsed_opt with
          | None -> ""
          | Some time -> Printf.sprintf "Time elapsed (ns): %f\n" time
        in
        Printf.sprintf "%sval %s :\n\t%s \n=\n\t %s" time_preface
          (Ast.ObjIdentifier.get_name id)
          (Ast.Typ.show typ) (Ast.Value.show v)
    | RecDefn ((id, typ), v, time_elapsed_opt) ->
        let time_preface =
          match time_elapsed_opt with
          | None -> ""
          | Some time -> Printf.sprintf "Time elapsed (ns): %f\n" time
        in
        Printf.sprintf "%sval rec %s:\n\t %s \n=\n\t %s" time_preface
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

let evaluate_top_level_defn ?(top_level_context = EvaluationContext.empty)
    ?(type_constr_context = TypeConstrContext.empty) ?(time_exec = false)
    top_level_defn =
  let open Or_error.Monad_infix in
  let time_elapsed_opt current_time time_exec =
    if time_exec then
      () |> Mtime_clock.now |> Mtime.span current_time |> Mtime.Span.to_float_ns
      |> Some
    else None
  in
  match top_level_defn with
  | Ast.TypedTopLevelDefn.Definition (typ, (id, _), e) ->
      let current_time = Mtime_clock.now () in
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun v ->
      let new_context =
        EvaluationContext.set top_level_context
          ~key:(Ast.ObjIdentifier.get_name id)
          ~data:{ is_rec = false; typ; value = v }
      in
      let elapsed = time_elapsed_opt current_time time_exec in
      Ok
        ( TopLevelEvaluationResult.Defn ((id, typ), v, elapsed),
          new_context,
          type_constr_context )
  | Ast.TypedTopLevelDefn.RecursiveDefinition (typ, (id, _), e) ->
      let current_time = Mtime_clock.now () in
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun v ->
      let new_entry : EvaluationContext.single_record =
        { is_rec = true; typ; value = v }
      in
      let key = Ast.ObjIdentifier.get_name id in
      let new_context =
        EvaluationContext.set top_level_context ~key ~data:new_entry
      in
      let elapsed = time_elapsed_opt current_time time_exec in
      Ok
        ( TopLevelEvaluationResult.RecDefn ((id, typ), v, elapsed),
          new_context,
          type_constr_context )
  | Ast.TypedTopLevelDefn.Expression (typ, e) ->
      let current_time = Mtime_clock.now () in
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun v ->
      let elapsed = time_elapsed_opt current_time time_exec in
      Ok
        ( TopLevelEvaluationResult.ExprValue (typ, v, elapsed),
          top_level_context,
          type_constr_context )
  | Ast.TypedTopLevelDefn.Directive d -> (
      match d with
      | Ast.Directive.Env ->
          let env = EvaluationContext.show top_level_context in
          let type_env = TypeConstrContext.show type_constr_context in
          let message =
            Printf.sprintf
              "ENV: \n\
               -------------------\n\
               \tVARIABLES: \n\
               %s\n\
               -------------------\n\
               \tDATATYPES: \n\
               %s"
              env type_env
          in
          Ok
            ( TopLevelEvaluationResult.Directive (d, message),
              top_level_context,
              type_constr_context )
      | Ast.Directive.Quit ->
          Ok
            ( TopLevelEvaluationResult.Directive (d, "User Quit."),
              top_level_context,
              type_constr_context )
      | Ast.Directive.Reset ->
          let message = "Cleared Env" in
          let new_env = EvaluationContext.empty in
          Ok
            ( TopLevelEvaluationResult.Directive (d, message),
              new_env,
              type_constr_context ))
  | Ast.TypedTopLevelDefn.DatatypeDecl (tid, constructor_type_list) ->
      TypeConstrContext.add_typ_from_decl type_constr_context
        (tid, constructor_type_list)
      >>= fun new_typ_context ->
      Ok
        ( TopLevelEvaluationResult.DatatypeDecl (tid, constructor_type_list),
          top_level_context,
          new_typ_context )

let rec evaluate_top_level_defns ?(top_level_context = EvaluationContext.empty)
    ?(type_constr_context = TypeConstrContext.empty) ?(time_exec = false)
    program =
  let open Or_error.Monad_infix in
  match program with
  | [] -> Ok ([], top_level_context, type_constr_context)
  | top :: tops -> (
      evaluate_top_level_defn ~top_level_context ~type_constr_context ~time_exec
        top
      >>= fun (top_level_result, new_context, new_typ_context) ->
      match top_level_result with
      | TopLevelEvaluationResult.Directive (Ast.Directive.Quit, _) ->
          Ok ([ top_level_result ], new_context, new_typ_context)
      | _ ->
          evaluate_top_level_defns ~top_level_context:new_context
            ~type_constr_context:new_typ_context ~time_exec tops
          >>= fun (evaluation_res, new_context, new_typ_context) ->
          Ok (top_level_result :: evaluation_res, new_context, new_typ_context))

let evaluate_program ?(top_level_context = EvaluationContext.empty)
    ?(type_constr_context = TypeConstrContext.empty) ?(time_exec = false)
    program =
  let open Or_error.Monad_infix in
  evaluate_top_level_defns ~top_level_context ~type_constr_context ~time_exec
    program
  >>= fun (evaluation_res, _, _) -> Ok evaluation_res
