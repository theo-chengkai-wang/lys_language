open Lys_ast
open Core
open Interpreter_common
open Lys_utils

let rec multi_step_reduce_with_step_count ~top_level_context
    ~type_constr_context ~expr =
  let open Or_error.Monad_infix in
  match expr with
  | Ast.Expr.Identifier id -> (
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
        >>= fun { rec_preface; value; _ } ->
        Ast.DeBruijnIndex.create 0 >>= fun debruijn_index ->
        match rec_preface with
        | [] -> Ok (value, 1)
        | [ (iddef, ev) ] ->
            multi_step_reduce_with_step_count ~top_level_context
              ~type_constr_context
              ~expr:
                (Ast.Expr.LetRec
                   ( iddef,
                     ev,
                     Ast.Expr.Identifier
                       (Ast.ObjIdentifier.of_string_and_index id_str
                          debruijn_index) ))
            >>= fun (v, cnt) -> Ok (v, cnt + 1)
        | iddef_ev_list ->
            multi_step_reduce_with_step_count ~top_level_context
              ~type_constr_context
              ~expr:
                (Ast.Expr.LetRecMutual
                   ( iddef_ev_list,
                     Ast.Expr.Identifier
                       (Ast.ObjIdentifier.of_string_and_index id_str
                          debruijn_index) ))
            >>= fun (v, cnt) -> Ok (v, cnt + 1)
        (* if EvaluationContext.is_not_rec { typ; rec_preface ; value } then
             (*Substitution -- no need to worry about De Bruijn indices as there is no way they can go wrong*)
             Ok (value, 1)
           else
             (* Recursion: handle once together. *) (*TODO: Change this to right definition*)
             Ast.DeBruijnIndex.create 0 >>= fun debruijn_index ->
             multi_step_reduce_with_step_count ~top_level_context
               ~type_constr_context
               ~expr:
                 (Ast.Expr.LetRec
                    ( (Ast.ObjIdentifier.of_string id_str, typ),
                      Ast.Value.to_expr value,
                      Ast.Expr.Identifier
                        (Ast.ObjIdentifier.of_string_and_index id_str
                           debruijn_index) ))
             >>= fun e -> *))
  | Ast.Expr.Constant c -> Ok (Ast.Value.Constant c, 0)
  | Ast.Expr.UnaryOp (op, expr) -> (
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr
      >>= fun (v, new_reduction_count) ->
      match (op, v) with
      | Ast.UnaryOperator.NEG, Ast.Value.Constant (Ast.Constant.Integer i) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Integer (-i)),
              new_reduction_count + 1 )
      | Ast.UnaryOperator.NOT, Ast.Value.Constant (Ast.Constant.Boolean b) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Boolean (not b)),
              new_reduction_count + 1 )
      | _, _ ->
          error
            "EvaluationError: type mismatch at Unary operator. [FATAL] should \
             not happen! Type check should have prevented this."
            (Ast.Expr.UnaryOp (op, expr))
            [%sexp_of: Ast.Expr.t])
  | Ast.Expr.BinaryOp (op, expr1, expr2) -> (
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:expr1
      >>= fun (v1, reduction_count1) ->
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:expr2
      >>= fun (v2, reduction_count2) ->
      let new_reduction_count = reduction_count1 + reduction_count2 in
      match (op, v1, v2) with
      | ( Ast.BinaryOperator.ADD,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Integer (i1 + i2)),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.SUB,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Integer (i1 - i2)),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.MUL,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Integer (i1 * i2)),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.DIV,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Integer (i1 / i2)),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.MOD,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Integer (i1 % i2)),
              new_reduction_count + 1 )
      | Ast.BinaryOperator.EQ, v1, v2 ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Boolean (Ast.Value.equal v1 v2)),
              new_reduction_count + 1 )
      | Ast.BinaryOperator.NEQ, v1, v2 ->
          Ok
            ( Ast.Value.Constant
                (Ast.Constant.Boolean (not (Ast.Value.equal v1 v2))),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.GTE,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Boolean (i1 >= i2)),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.GT,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Boolean (i1 > i2)),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.LT,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Boolean (i1 < i2)),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.LTE,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Boolean (i1 <= i2)),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.AND,
          Ast.Value.Constant (Ast.Constant.Boolean i1),
          Ast.Value.Constant (Ast.Constant.Boolean i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Boolean (i1 && i2)),
              new_reduction_count + 1 )
      | ( Ast.BinaryOperator.OR,
          Ast.Value.Constant (Ast.Constant.Boolean i1),
          Ast.Value.Constant (Ast.Constant.Boolean i2) ) ->
          Ok
            ( Ast.Value.Constant (Ast.Constant.Boolean (i1 || i2)),
              new_reduction_count + 1 )
      | _, _, _ ->
          error
            "EvaluationError: type mismatch at Binary operator. [FATAL] should \
             not happen! Type check should have prevented this."
            (Ast.Expr.BinaryOp (op, expr1, expr2))
            [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Prod exprs ->
      List.map exprs ~f:(fun expr ->
          multi_step_reduce_with_step_count ~top_level_context
            ~type_constr_context ~expr)
      |> Or_error.combine_errors
      >>= fun values_cnts_zipped ->
      let values, reduction_counts = List.unzip values_cnts_zipped in
      (* Sum the counts *)
      let new_reduction_count =
        List.fold reduction_counts ~init:0 ~f:(fun acc c -> acc + c)
      in
      (*Only Congruence*)
      Ok (Ast.Value.Prod values, new_reduction_count)
  | Ast.Expr.Nth (expr, i) -> (
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr
      >>= fun (v, new_reduction_count) ->
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
          | Some v -> Ok (v, new_reduction_count + 1))
      | _ ->
          error
            "EvaluationError: type mismatch at Nth. [FATAL] should not happen! \
             Type check should have prevented this."
            (Ast.Expr.Nth (expr, i))
            [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Left (t1, t2, expr) ->
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr
      >>= fun (v, new_reduction_count) ->
      Ok (Ast.Value.Left (t1, t2, v), new_reduction_count)
      (* We don't run RT type check here. *)
  | Ast.Expr.Right (t1, t2, expr) ->
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr
      >>= fun (v, new_reduction_count) ->
      Ok (Ast.Value.Right (t1, t2, v), new_reduction_count)
      (* We don't run RT type check here. *)
  | Ast.Expr.Case (e, (id1, _), e1, (id2, _), e2) ->
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e
      >>= fun (v, reduction_count1) ->
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
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:new_expr
      >>= fun (v, reduction_count2) ->
      Ok (v, reduction_count1 + reduction_count2 + 1)
  | Ast.Expr.Lambda (iddef, e) -> Ok (Ast.Value.Lambda (iddef, e), 0)
  | Ast.Expr.Application (e1, e2) -> (
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e1
      >>= fun (v1, reduction_count1) ->
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e2
      >>= fun (v2, reduction_count2) ->
      let new_reduction_count = reduction_count1 + reduction_count2 in
      match v1 with
      | Ast.Value.Lambda ((id, _), e) ->
          Substitutions.substitute (Ast.Value.to_expr v2) id e
          >>= fun new_expr ->
          multi_step_reduce_with_step_count ~top_level_context
            ~type_constr_context ~expr:new_expr
          >>= fun (v3, reduction_count3) ->
          (*Account for third call and for the reduction step undertaken*)
          let new_reduction_count =
            new_reduction_count + 1 + reduction_count3
          in
          Ok (v3, new_reduction_count)
      | _ ->
          error
            "EvaluationError: type mismatch at Lambda. [FATAL] should not \
             happen! Type check should have prevented this."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.IfThenElse (b, e1, e2) -> (
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:b
      >>= fun (bv, reduction_count1) ->
      match bv with
      | Ast.Value.Constant (Ast.Constant.Boolean v) ->
          if v then
            multi_step_reduce_with_step_count ~top_level_context
              ~type_constr_context ~expr:e1
            >>= fun (v, reduction_count2) ->
            Ok (v, reduction_count1 + 1 + reduction_count2)
          else
            multi_step_reduce_with_step_count ~top_level_context
              ~type_constr_context ~expr:e2
            >>= fun (v, reduction_count2) ->
            Ok (v, reduction_count1 + 1 + reduction_count2)
      | _ ->
          error
            "EvaluationError: type mismatch at IfThenElse. [FATAL] should not \
             happen! Type check should have prevented this."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.LetBinding ((id, _), e, e2) ->
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e
      >>= fun (ev, reduction_count1) ->
      Substitutions.substitute (Ast.Value.to_expr ev) id e2 >>= fun e ->
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e
      >>= fun (v, reduction_count2) ->
      Ok (v, reduction_count1 + 1 + reduction_count2)
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
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e
      >>= fun (v, reduction_count1) ->
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
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:ev2
      >>= fun (v, reduction_count2) ->
      Ok (v, reduction_count1 + 1 + reduction_count2)
  | Ast.Expr.LetRecMutual (iddef_e_list, e2) ->
      (*
         TODO: Check if this is right
         Explanation: exactly the same thing as LetRec case, but with ALL the defns with it.
         let rec f = v and g = v2 in e  ~~~~~> [([(let rec f = v and g = v2 in f)/f; (let rec f = v and g = v2 in g)/g]v)/f; (...v2)/g]e
      *)
      Ast.DeBruijnIndex.create 0 >>= fun debruijn_index_0 ->
      (* First reduce everything and get a preface: the iddef_e_list in LetRecMutual (iddef_e_list) *)
      List.fold iddef_e_list
        ~init:(Ok ([], 0))
        ~f:(fun acc ((id, typ), e) ->
          acc >>= fun (acc_iddef_e_list, acc_step_count) ->
          multi_step_reduce_with_step_count ~top_level_context
            ~type_constr_context ~expr:e
          >>= fun (v, step_count) ->
          Ok
            ( ((id, typ), Ast.Value.to_expr v) :: acc_iddef_e_list,
              step_count + acc_step_count ))
      >>= fun (new_iddef_e_list, step_count) ->
      let new_iddef_e_list = List.rev new_iddef_e_list in
      (* Put it back in the right order *)
      let iddefs, exprs =
        List.map new_iddef_e_list ~f:(fun ((id, typ), _) ->
            ( (id, typ),
              Ast.Expr.LetRecMutual
                ( new_iddef_e_list,
                  Ast.Expr.Identifier
                    (Ast.ObjIdentifier.of_string_and_index
                       (Ast.ObjIdentifier.get_name id)
                       debruijn_index_0) ) ))
        |> List.unzip
      in
      List.map new_iddef_e_list ~f:(fun (_, e) ->
          Substitutions.sim_substitute exprs iddefs e)
      |> Or_error.combine_errors
      >>= fun substituted_expr_list ->
      Substitutions.sim_substitute substituted_expr_list iddefs e2
      >>= fun ev2 ->
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:ev2
      >>= fun (v, step_count_2) -> Ok (v, step_count + 1 + step_count_2)
  | Ast.Expr.Box (ctx, e) -> Ok (Ast.Value.Box (ctx, e), 0)
  | Ast.Expr.LetBox (metaid, e, e2) -> (
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e
      >>= fun (box_v, reduction_count1) ->
      match box_v with
      | Ast.Value.Box (ctx, e_box) ->
          Substitutions.meta_substitute ctx e_box metaid e2 |> fun or_error ->
          Or_error.tag_arg or_error
            "EvaluationError: Meta substitution error: metaid, e->v, ctx, v"
            (metaid, box_v, ctx, e2)
            [%sexp_of:
              Ast.MetaIdentifier.t * Ast.Value.t * Ast.Context.t * Ast.Expr.t]
          >>= fun res_meta_sub ->
          multi_step_reduce_with_step_count ~top_level_context
            ~type_constr_context ~expr:res_meta_sub
          >>= fun (v, reduction_count2) ->
          Ok (v, reduction_count1 + 1 + reduction_count2)
      | _ ->
          error
            "EvaluationError: type mismatch at LetBox. [FATAL] should not \
             happen! Type check should have prevented this."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Closure (_, _) ->
      error "EvaluationError: One should never have to evaluate a raw closure."
        expr [%sexp_of: Ast.Expr.t]
  | Ast.Expr.Lift (_, expr) ->
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr
      >>= fun (v, reduction_count1) ->
      let expr_v = Ast.Value.to_expr v in
      Ok (Ast.Value.Box ([], expr_v), reduction_count1 + 1)
  | Ast.Expr.Constr (constr, e_opt) -> (
      if
        Option.is_none
          (TypeConstrContext.get_typ_from_constr type_constr_context constr)
      then
        error "EvaluationError: FATAL: constructor undefined" constr
          [%sexp_of: Ast.Constructor.t]
      else
        match e_opt with
        | None -> Ok (Ast.Value.Constr (constr, None), 0)
        | Some e ->
            multi_step_reduce_with_step_count ~top_level_context
              ~type_constr_context ~expr:e
            >>= fun (v, reduction_count) ->
            Ok (Ast.Value.Constr (constr, Some v), reduction_count))
  | Ast.Expr.Match (e, pattn_expr_list) -> (
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e
      >>= fun (v, reduction_count1) ->
      let match_pattn_expr v (pattn, expr) =
        (* Processes the pattern: if not matched, return None; if matched return Some (Value.t Or_error.t) *)
        match (pattn, v) with
        | Ast.Pattern.Wildcard, _ ->
            multi_step_reduce_with_step_count ~top_level_context
              ~type_constr_context ~expr
            |> Some
        | Ast.Pattern.Id id, _ ->
            let expr_of_v = Ast.Value.to_expr v in
            Substitutions.substitute expr_of_v id expr
            >>= (fun substituted_expr ->
                  multi_step_reduce_with_step_count ~top_level_context
                    ~type_constr_context ~expr:substituted_expr)
            |> Some
        | Ast.Pattern.Inl id, Ast.Value.Left (_, _, v) ->
            let expr_of_v = Ast.Value.to_expr v in
            Substitutions.substitute expr_of_v id expr
            >>= (fun substituted_expr ->
                  multi_step_reduce_with_step_count ~top_level_context
                    ~type_constr_context ~expr:substituted_expr)
            |> Some
        | Ast.Pattern.Inr id, Ast.Value.Right (_, _, v) ->
            let expr_of_v = Ast.Value.to_expr v in
            Substitutions.substitute expr_of_v id expr
            >>= (fun substituted_expr ->
                  multi_step_reduce_with_step_count ~top_level_context
                    ~type_constr_context ~expr:substituted_expr)
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
                  multi_step_reduce_with_step_count ~top_level_context
                    ~type_constr_context ~expr:substituted_expr)
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
              multi_step_reduce_with_step_count ~top_level_context
                ~type_constr_context ~expr
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
                    multi_step_reduce_with_step_count ~top_level_context
                      ~type_constr_context ~expr:substituted_expr)
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
                    multi_step_reduce_with_step_count ~top_level_context
                      ~type_constr_context ~expr:substituted_expr)
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
      | Some res ->
          res >>= fun (v, reduction_count2) ->
          Ok (v, reduction_count1 + reduction_count2 + 1))
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

let evaluate_top_level_defn_with_step_count
    ?(top_level_context = EvaluationContext.empty)
    ?(type_constr_context = TypeConstrContext.empty) ?(time_exec = false)
    ?(show_reduction_steps = false) top_level_defn =
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
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e
      >>= fun (v, reduction_count) ->
      let new_context =
        EvaluationContext.set top_level_context
          ~key:(Ast.ObjIdentifier.get_name id)
          ~data:{ rec_preface = []; typ; value = v }
      in
      let elapsed = time_elapsed_opt current_time time_exec in
      let reduction_count_opt =
        if show_reduction_steps then Some reduction_count else None
      in
      Ok
        ( TopLevelEvaluationResult.Defn
            ((id, typ), v, elapsed, reduction_count_opt, None),
          new_context,
          type_constr_context )
  | Ast.TypedTopLevelDefn.RecursiveDefinition (typ, (id, _), e) ->
      let current_time = Mtime_clock.now () in
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e
      >>= fun (v, reduction_count) ->
      let new_entry : EvaluationContext.single_record =
        { rec_preface = [ ((id, typ), Ast.Value.to_expr v) ]; typ; value = v }
      in
      let key = Ast.ObjIdentifier.get_name id in
      let new_context =
        EvaluationContext.set top_level_context ~key ~data:new_entry
      in
      let elapsed = time_elapsed_opt current_time time_exec in
      let reduction_count_opt =
        if show_reduction_steps then Some reduction_count else None
      in
      Ok
        ( TopLevelEvaluationResult.RecDefn
            ((id, typ), v, elapsed, reduction_count_opt, None),
          new_context,
          type_constr_context )
  | Ast.TypedTopLevelDefn.MutualRecursiveDefinition iddef_e_list ->
      List.map iddef_e_list ~f:(fun (_, iddef, e) ->
          let current_time = Mtime_clock.now () in
          multi_step_reduce_with_step_count ~top_level_context
            ~type_constr_context ~expr:e
          >>= fun (v, cnt) ->
          let elapsed = time_elapsed_opt current_time time_exec in
          let reduction_count_opt =
            if show_reduction_steps then Some cnt else None
          in
          Ok (iddef, v, elapsed, reduction_count_opt, None))
      |> Or_error.combine_errors
      (*Get final result*)
      >>= fun iddef_v_stats_list ->
      (* Add to context *)
      let iddef_v_list =
        List.map iddef_v_stats_list ~f:(fun (iddef, v, _, _, _) -> (iddef, v))
      in
      let rec_preface =
        List.map iddef_v_list ~f:(fun (iddef, v) ->
            (iddef, Ast.Value.to_expr v))
      in
      let kv_pairs =
        List.map iddef_v_list ~f:(fun ((id, typ), v) ->
            let new_entry : EvaluationContext.single_record =
              { rec_preface; typ; value = v }
            in
            (Ast.ObjIdentifier.get_name id, new_entry))
      in
      let new_context = EvaluationContext.set_all top_level_context kv_pairs in
      Ok
        ( TopLevelEvaluationResult.MutRecDefn iddef_v_stats_list,
          new_context,
          type_constr_context )
  | Ast.TypedTopLevelDefn.Expression (typ, e) ->
      let current_time = Mtime_clock.now () in
      multi_step_reduce_with_step_count ~top_level_context ~type_constr_context
        ~expr:e
      >>= fun (v, reduction_count) ->
      let elapsed = time_elapsed_opt current_time time_exec in
      let reduction_count_opt =
        if show_reduction_steps then Some reduction_count else None
      in
      Ok
        ( TopLevelEvaluationResult.ExprValue
            (typ, v, elapsed, reduction_count_opt, None),
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

let rec multi_step_reduce ~top_level_context ~type_constr_context ~expr =
  let open Or_error.Monad_infix in
  match expr with
  | Ast.Expr.Identifier id -> (
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
        >>= fun { rec_preface; value; _ } ->
        Ast.DeBruijnIndex.create 0 >>= fun debruijn_index_0 ->
        match rec_preface with
        | [] -> Ok value
        | [ (iddef, ev) ] ->
            (* Rec *)
            multi_step_reduce ~top_level_context ~type_constr_context
              ~expr:
                (Ast.Expr.LetRec
                   ( iddef,
                     ev,
                     Ast.Expr.Identifier
                       (Ast.ObjIdentifier.of_string_and_index id_str
                          debruijn_index_0) ))
            >>= fun v -> Ok v
        | iddef_ev_list ->
            (* Mutual Rec *)
            multi_step_reduce ~top_level_context ~type_constr_context
              ~expr:
                (Ast.Expr.LetRecMutual
                   ( iddef_ev_list,
                     Ast.Expr.Identifier
                       (Ast.ObjIdentifier.of_string_and_index id_str
                          debruijn_index_0) ))
        (* if EvaluationContext.is_not_rec { typ; rec_preface; value } then
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
                           debruijn_index) )) *))
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
  | Ast.Expr.LetRecMutual (iddef_e_list, e2) ->
      (*
           Explanation: exactly the same thing as LetRec case, but with ALL the defns with it.
           let rec f = v and g = v2 in e2  ~~~~~> [([(let rec f = v and g = v2 in f)/f; (let rec f = v and g = v2 in g)/g]v)/f; (...v2)/g]e2
        *)
      Ast.DeBruijnIndex.create 0 >>= fun debruijn_index_0 ->
      (* First reduce everything and get a preface: the iddef_e_list in LetRecMutual (iddef_e_list) *)
      List.map iddef_e_list ~f:(fun ((id, typ), e) ->
          multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
          >>= fun v -> Ok ((id, typ), Ast.Value.to_expr v))
      |> Or_error.combine_errors
      >>= fun new_iddef_e_list ->
      (* Put it back in the right order *)
      let new_iddef_e_list = List.rev new_iddef_e_list in
      (* \foreach v. [(let rec f = v and g = v2 in f)/f; (let rec f = v and g = v2 in g)/g]v *)
      let iddefs, exprs =
        List.map new_iddef_e_list ~f:(fun ((id, typ), _) ->
            ( (id, typ),
              Ast.Expr.LetRecMutual
                ( new_iddef_e_list,
                  Ast.Expr.Identifier
                    (Ast.ObjIdentifier.of_string_and_index
                       (Ast.ObjIdentifier.get_name id)
                       debruijn_index_0) ) ))
        |> List.unzip
      in
      List.map new_iddef_e_list ~f:(fun (_, e) ->
          Substitutions.sim_substitute exprs iddefs e)
      |> Or_error.combine_errors
      >>= fun substituted_expr_list ->
      (* [substituted_v/f; substituted_v2/g]e2 *)
      Substitutions.sim_substitute substituted_expr_list iddefs e2
      >>= fun ev2 ->
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
          ~data:{ rec_preface = []; typ; value = v }
      in
      let elapsed = time_elapsed_opt current_time time_exec in

      Ok
        ( TopLevelEvaluationResult.Defn ((id, typ), v, elapsed, None, None),
          new_context,
          type_constr_context )
  | Ast.TypedTopLevelDefn.RecursiveDefinition (typ, (id, _), e) ->
      let current_time = Mtime_clock.now () in
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun v ->
      let new_entry : EvaluationContext.single_record =
        { rec_preface = [ ((id, typ), e) ]; typ; value = v }
      in
      let key = Ast.ObjIdentifier.get_name id in
      let new_context =
        EvaluationContext.set top_level_context ~key ~data:new_entry
      in
      let elapsed = time_elapsed_opt current_time time_exec in
      Ok
        ( TopLevelEvaluationResult.RecDefn ((id, typ), v, elapsed, None, None),
          new_context,
          type_constr_context )
  | Ast.TypedTopLevelDefn.MutualRecursiveDefinition iddef_e_list ->
      List.map iddef_e_list ~f:(fun (_, iddef, e) ->
          let current_time = Mtime_clock.now () in
          multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
          >>= fun v ->
          let elapsed = time_elapsed_opt current_time time_exec in
          Ok (iddef, v, elapsed, None, None))
      |> Or_error.combine_errors
      (*Get final result*)
      >>= fun iddef_v_stats_list ->
      (* Add to context *)
      let iddef_v_list =
        List.map iddef_v_stats_list ~f:(fun (iddef, v, _, _, _) -> (iddef, v))
      in
      let rec_preface =
        List.map iddef_v_list ~f:(fun (iddef, v) ->
            (iddef, Ast.Value.to_expr v))
      in
      let kv_pairs =
        List.map iddef_v_list ~f:(fun ((id, typ), v) ->
            let new_entry : EvaluationContext.single_record =
              { rec_preface; typ; value = v }
            in
            (Ast.ObjIdentifier.get_name id, new_entry))
      in
      let new_context = EvaluationContext.set_all top_level_context kv_pairs in
      Ok
        ( TopLevelEvaluationResult.MutRecDefn iddef_v_stats_list,
          new_context,
          type_constr_context )
  | Ast.TypedTopLevelDefn.Expression (typ, e) ->
      let current_time = Mtime_clock.now () in
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      >>= fun v ->
      let elapsed = time_elapsed_opt current_time time_exec in
      Ok
        ( TopLevelEvaluationResult.ExprValue (typ, v, elapsed, None, None),
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
