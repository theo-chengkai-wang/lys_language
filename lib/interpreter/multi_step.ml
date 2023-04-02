open Lys_ast
open Core
open Interpreter_common
open Lys_utils

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
      | Ast.UnaryOperator.DEREF, Ast.Value.Constant (Ast.Constant.Reference r)
        ->
          Ok !r
      | _, _ ->
          error
            "EvaluationError: type mismatch at Unary operator. [FATAL] should \
             not happen! Type check should have prevented this."
            (Ast.Expr.UnaryOp (op, expr))
            [%sexp_of: Ast.Expr.t])
  | Ast.Expr.BinaryOp (op, expr1, expr2) -> (
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:expr1
      >>= fun v1 ->
      (*Cut exection here and change semantics.*)
      match (op, v1) with
      | Ast.BinaryOperator.SEQ, Ast.Value.Constant Ast.Constant.Unit ->
          multi_step_reduce ~top_level_context ~type_constr_context ~expr:expr2
      | Ast.BinaryOperator.AND, Ast.Value.Constant (Ast.Constant.Boolean false)
        ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean false))
      | Ast.BinaryOperator.OR, Ast.Value.Constant (Ast.Constant.Boolean true) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean true))
      | _, _ -> (
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
              (* DEBUG print_endline (Printf.sprintf "Value 1: %s \n Value 2: %s \n" (Ast.Value.show v1) (Ast.Value.show v2)); *)
              Ok
                (Ast.Value.Constant
                   (Ast.Constant.Boolean (Ast.Value.equal v1 v2)))
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
          | ( Ast.BinaryOperator.CHARSTRINGCONCAT,
              Ast.Value.Constant (Ast.Constant.Character c),
              Ast.Value.Constant (Ast.Constant.String s) ) ->
              Ok
                (Ast.Value.Constant (Ast.Constant.String (String.of_char c ^ s)))
          | ( Ast.BinaryOperator.STRINGCONCAT,
              Ast.Value.Constant (Ast.Constant.String s),
              Ast.Value.Constant (Ast.Constant.String s2) ) ->
              Ok (Ast.Value.Constant (Ast.Constant.String (s ^ s2)))
          | ( Ast.BinaryOperator.ASSIGN,
              Ast.Value.Constant (Ast.Constant.Reference r1),
              v ) ->
              r1 := v;
              Ok (Ast.Value.Constant Ast.Constant.Unit)
          | _, _, _ ->
              error
                "EvaluationError: type mismatch at Binary operator. [FATAL] \
                 should not happen! Type check should have prevented this."
                (Ast.Expr.BinaryOp (op, expr1, expr2))
                [%sexp_of: Ast.Expr.t]))
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
      (* Debug code here
         let () = if String.equal (Ast.MetaIdentifier.get_name metaid) ("res2_") || String.equal (Ast.MetaIdentifier.get_name metaid) ("res1_") then print_endline (Ast.Expr.show expr) in *)
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
        | Ast.Pattern.String s, Ast.Value.Constant (Ast.Constant.String s2) ->
            if String.equal s s2 then
              multi_step_reduce ~top_level_context ~type_constr_context ~expr
              |> Some
            else None
        | ( Ast.Pattern.ConcatCharString (cid, sid),
            Ast.Value.Constant (Ast.Constant.String s) ) ->
            if String.is_empty s then None
            else
              let zipped_list =
                [
                  ( Ast.Expr.Constant (Ast.Constant.Character (String.get s 0)),
                    Ast.ObjIdentifier.get_name cid );
                  ( Ast.Expr.Constant
                      (Ast.Constant.String (String.drop_prefix s 1)),
                    Ast.ObjIdentifier.get_name sid );
                ]
              in
              Substitutions.sim_substitute_from_zipped_list zipped_list expr
              >>= (fun substituted_expr ->
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
  | Ast.Expr.EValue v -> Ok v
  | Ast.Expr.Ref e ->
      multi_step_reduce ~top_level_context ~type_constr_context ~expr:e
      (* Reference creation *)
      >>= fun v -> Ok (Ast.Value.Constant (Ast.Constant.Reference (ref v)))
  | Ast.Expr.While (p, e) ->
      multi_step_reduce ~top_level_context ~type_constr_context
        ~expr:
          (Ast.Expr.IfThenElse
             ( p,
               Ast.Expr.BinaryOp
                 (Ast.BinaryOperator.SEQ, e, Ast.Expr.While (p, e)),
               Ast.Expr.Constant Ast.Constant.Unit ))
      |> fun or_error ->
      Or_error.tag_arg or_error
        "EvaluationError: FATAL: while loop predicate is not a boolean \
         (predicate)"
        p [%sexp_of: Ast.Expr.t]

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
  | Ast.TypedTopLevelDefn.DatatypeDecl id_constr_typ_list_list ->
      List.fold id_constr_typ_list_list ~init:(Ok type_constr_context)
        ~f:(fun acc (tid, constructor_type_list) ->
          acc >>= fun new_typ_ctx ->
          TypeConstrContext.add_typ_from_decl new_typ_ctx
            (tid, constructor_type_list))
      >>= fun new_typ_context ->
      Ok
        ( TopLevelEvaluationResult.DatatypeDecl id_constr_typ_list_list,
          top_level_context,
          new_typ_context )
