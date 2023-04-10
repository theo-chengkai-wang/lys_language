open Core
open Lys_ast

(*
   This definition takes an id (string) and a depth (of the tree) with depth 0 at root. We recurse down the tree adding depth
    when going through a binder, and shift the tree BY to the current tree depth when substituting in. In other words,
    we assume that in [e/z]e', e is closed at depth 0 (so any free variable it contains have De Bruijn indices with depth 0).

    ATTENTION: ASSUMPTION: (fun x -> e1) e2 => [e2/x]e1
    - e1 with De Bruijn index of 0 meaning x, at level 1.
    - e2 with all the indexes having 0 meaning the thing above x.
    - if e1 = .... x[0] ...y[3]...fun z -> z[0]...x[1]...y[4]... and e2 = ... y[2] ... fun x -> x[0]..y[3].. 
        then we want to get ... (... y[2] ... fun x -> x[0]...y[3]..) ...y[2]...fun z -> z[0]... (e2[shift by 1] = ...y[3] ..fun x->x[0]..y[4].) y[3]...
    - So what we really want is to shift everything bound outside of (fun x -> e1) to be shifted back by 1, and 
    that x[0] be substituted by e2, shifted by the current depth wrt the application.
    - So
        - in e2, we'd want every free variable (i.e. every variable such as the De Bruijn index is bigger or equal to the depth) 
        to be shifted by the current depth of the identifier where it's sub-ed in. 
        - In e1, we'd want every free thing to be shifted back by 1. UPDATE: No real need to check this, because ALL FREE VARIABLES WOULD HAVE
        BEEN SUBSTITUTED IN! We never really perform substitution under a binder (meaning we never rly have substitution on expressions containing free 
        variables). However I'll leave this here for generality.
*)

let equal_id_str_depth obj_id id_str de_bruijn_int =
  (*Checks whether the given object id corresponds to the id and the de bruijn index given*)
  de_bruijn_int |> Ast.DeBruijnIndex.create |> Or_error.ok
  |> Option.value ~default:Ast.DeBruijnIndex.none
  |> Ast.ObjIdentifier.of_string_and_index id_str
  |> Ast.ObjIdentifier.equal obj_id

let rec substitute_aux expr_subst_for id_str current_depth current_meta_depth
    expr_subst_in =
  (* For now, assumes that expr_subst_for is CLOSED for performance reasons. *)
  let open Or_error.Monad_infix in
  match expr_subst_in with
  | Ast.Expr.Identifier oid ->
      if not (equal_id_str_depth oid id_str current_depth) then
        (*Shift free variables back*)
        (*We only shift something if its index is bigger-eq than the depth.
            E.g. if we have lambda x. (y: 1) then we want to shift y to 0.
          This is because we want to avoid substituting when we have multiple identifiers at the same depth
          with the same index*)
        Ast.ObjIdentifier.shift oid ~depth:current_depth ~offset:(-1)
        >>= fun oid -> Ok (Ast.Expr.Identifier oid)
      else
        (*Shift expression with free variables forth. We start from 0 as this is the assumption where we are at level 0 for
             the expr to be substituted in.
          Do that for meta variables too.
        *)
        (*TODO: Decide if this shall be a definitive change*)
        (* Ast.Expr.shift_indices expr_subst_for ~obj_depth:0 ~meta_depth:0
             ~obj_offset:current_depth ~meta_offset:current_meta_depth
           |> Or_error.tag
                ~tag:
                  (Printf.sprintf
                     "SubstitutionError: Index shifting failed when substituting \
                      expr %s for %s"
                     (Ast.Expr.show expr_subst_for)
                     id_str) *)
        Ok expr_subst_for
  | Ast.Expr.Constant c -> Ok (Ast.Expr.Constant c)
  | Ast.Expr.UnaryOp (op, expr) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.UnaryOp (op, expr))
  | Ast.Expr.BinaryOp (op, expr, expr2) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth expr
      >>= fun expr ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth
        expr2
      >>= fun expr2 -> Ok (Ast.Expr.BinaryOp (op, expr, expr2))
  | Ast.Expr.Prod exprs ->
      List.map exprs
        ~f:
          (substitute_aux expr_subst_for id_str current_depth current_meta_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Prod exprs)
  (* | Ast.Expr.Fst expr ->
         substitute_aux expr_subst_for id_str current_depth expr >>= fun expr ->
         Ok (Ast.Expr.Fst expr)
     | Ast.Expr.Snd expr ->
         substitute_aux expr_subst_for id_str current_depth expr >>= fun expr ->
         Ok (Ast.Expr.Snd expr) *)
  | Ast.Expr.Nth (expr, i) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.Nth (expr, i))
  | Ast.Expr.Left (t1, t2, expr) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.Left (t1, t2, expr))
  | Ast.Expr.Right (t1, t2, expr) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.Right (t1, t2, expr))
  | Ast.Expr.Case (e, iddef1, e1, iddef2, e2) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e
      >>= fun e ->
      substitute_aux expr_subst_for id_str (current_depth + 1)
        current_meta_depth e1
      >>= fun e1 ->
      substitute_aux expr_subst_for id_str (current_depth + 1)
        current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.Case (e, iddef1, e1, iddef2, e2))
  | Ast.Expr.Lambda (iddef, e) ->
      substitute_aux expr_subst_for id_str (current_depth + 1)
        current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.Lambda (iddef, e))
  | Ast.Expr.Application (e1, e2) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e1
      >>= fun e1 ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.Application (e1, e2))
  | Ast.Expr.IfThenElse (b, e1, e2) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth b
      >>= fun b ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e1
      >>= fun e1 ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.IfThenElse (b, e1, e2))
  | Ast.Expr.LetBinding (iddef, e, e2) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e
      >>= fun e ->
      substitute_aux expr_subst_for id_str (current_depth + 1)
        current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetBinding (iddef, e, e2))
  | Ast.Expr.LetRec (iddef, e, e2) ->
      substitute_aux expr_subst_for id_str (current_depth + 1)
        current_meta_depth e
      >>= fun e ->
      substitute_aux expr_subst_for id_str (current_depth + 1)
        current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetRec (iddef, e, e2))
  | Ast.Expr.LetRecMutual (iddef_e_list, e2) ->
      List.map iddef_e_list ~f:(fun (iddef, e) ->
          substitute_aux expr_subst_for id_str (current_depth + 1)
            current_meta_depth e
          >>= fun e -> Ok (iddef, e))
      |> Or_error.combine_errors
      >>= fun iddef_e_list ->
      substitute_aux expr_subst_for id_str (current_depth + 1)
        current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetRecMutual (iddef_e_list, e2))
  | Ast.Expr.Box (tvctx, ctx, e) -> Ok (Ast.Expr.Box (tvctx, ctx, e))
  | Ast.Expr.LetBox (metaid, e, e2) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e
      >>= fun e ->
      substitute_aux expr_subst_for id_str current_depth
        (current_meta_depth + 1) e2
      >>= fun e2 -> Ok (Ast.Expr.LetBox (metaid, e, e2))
  | Ast.Expr.Closure (metaid, typs, exprs) ->
      exprs
      |> List.map
           ~f:
             (substitute_aux expr_subst_for id_str current_depth
                current_meta_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Closure (metaid, typs, exprs))
  | Ast.Expr.Constr (tid, e_opt) -> (
      match e_opt with
      | None -> Ok (Ast.Expr.Constr (tid, e_opt))
      | Some e ->
          substitute_aux expr_subst_for id_str current_depth current_meta_depth
            e
          >>= fun e -> Ok (Ast.Expr.Constr (tid, Some e)))
  | Ast.Expr.Match (e, pattn_expr_list) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e
      >>= fun e ->
      List.map pattn_expr_list ~f:(fun (pattn, expr) ->
          let binders = Ast.Pattern.get_binders pattn in
          let new_depth =
            if List.is_empty binders then current_depth else current_depth + 1
          in
          substitute_aux expr_subst_for id_str new_depth current_meta_depth expr
          >>= fun expr -> Ok (pattn, expr))
      |> Or_error.combine_errors
      >>= fun pattn_expr_list -> Ok (Ast.Expr.Match (e, pattn_expr_list))
  | Ast.Expr.Lift (typ, expr) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.Lift (typ, expr))
  | Ast.Expr.EValue v ->
      Ast.Value.to_expr v
      |> substitute_aux expr_subst_for id_str current_depth current_meta_depth
  | Ast.Expr.Ref e ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.Ref e)
  | Ast.Expr.While (p, e) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth p
      >>= fun p ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.While (p, e))
  | Ast.Expr.Array exprs ->
      List.map exprs
        ~f:
          (substitute_aux expr_subst_for id_str current_depth current_meta_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Array exprs)
  | Ast.Expr.ArrayAssign (e1, e2, e3) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e1
      >>= fun e1 ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e2
      >>= fun e2 ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e3
      >>= fun e3 -> Ok (Ast.Expr.ArrayAssign (e1, e2, e3))
  | Ast.Expr.BigLambda (v, e) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.BigLambda (v, e))
  | Ast.Expr.TypeApply (e, t) ->
      substitute_aux expr_subst_for id_str current_depth current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.TypeApply (e, t))

let substitute expr_subst_for id expr_subst_in =
  (*Assume that the id has De Bruijn index 0*)
  let id_str = Ast.ObjIdentifier.get_name id in
  let current_depth = 0 in
  let current_meta_depth = 0 in
  substitute_aux expr_subst_for id_str current_depth current_meta_depth
    expr_subst_in

let rec sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
    current_type_depth expr_subst_in =
  (*Assume that all the ids have De Bruijn index 0*)
  let open Or_error.Monad_infix in
  match expr_subst_in with
  | Ast.Expr.Identifier oid -> (
      let id_to_check = Ast.ObjIdentifier.get_name oid in
      let index_to_check =
        Ast.DeBruijnIndex.value
          (Ast.ObjIdentifier.get_debruijn_index oid)
          ~default:(-1)
        (*because DB index can't be negative*)
      in
      if not (index_to_check = current_depth) then Ok (Ast.Expr.Identifier oid)
      else
        let expr_id_opt =
          List.find zipped_exprs_ids ~f:(fun (_, id) ->
              String.equal id id_to_check)
        in
        match expr_id_opt with
        | None ->
            Ast.ObjIdentifier.shift oid ~depth:current_depth ~offset:(-1)
            >>= fun oid -> Ok (Ast.Expr.Identifier oid)
        | Some (expr, id) ->
            (* In the expr to sub in, shift obj DB indices by the current_depth to preserve consistency of the DB indices.
               Shift meta-indices too*)
            Ast.Expr.shift_indices expr ~obj_depth:0 ~meta_depth:0 ~type_depth:0
              ~obj_offset:current_depth ~meta_offset:current_meta_depth
              ~type_offset:current_type_depth
            (* TODO: Correct here to have proper type substitution *)
            |> Or_error.tag
                 ~tag:
                   (Printf.sprintf
                      "SimultaneousSubstitutionError: Index shifting failed \
                       when substituting expr %s for %s"
                      (Ast.Expr.show expr) id))
  | Ast.Expr.Constant c -> Ok (Ast.Expr.Constant c)
  | Ast.Expr.UnaryOp (op, expr) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth expr
      >>= fun expr -> Ok (Ast.Expr.UnaryOp (op, expr))
  | Ast.Expr.BinaryOp (op, expr, expr2) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth expr
      >>= fun expr ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth expr2
      >>= fun expr2 -> Ok (Ast.Expr.BinaryOp (op, expr, expr2))
  | Ast.Expr.Prod exprs ->
      List.map exprs
        ~f:
          (sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
             current_type_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Prod exprs)
  (* | Ast.Expr.Fst expr ->
         sim_substitute_aux zipped_exprs_ids current_depth expr >>= fun expr ->
         Ok (Ast.Expr.Fst expr)
     | Ast.Expr.Snd expr ->
         sim_substitute_aux zipped_exprs_ids current_depth expr >>= fun expr ->
         Ok (Ast.Expr.Snd expr) *)
  | Ast.Expr.Nth (expr, i) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth expr
      >>= fun expr -> Ok (Ast.Expr.Nth (expr, i))
  | Ast.Expr.Left (t1, t2, expr) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth expr
      >>= fun expr -> Ok (Ast.Expr.Left (t1, t2, expr))
  | Ast.Expr.Right (t1, t2, expr) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth expr
      >>= fun expr -> Ok (Ast.Expr.Right (t1, t2, expr))
  | Ast.Expr.Case (e, iddef1, e1, iddef2, e2) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e
      >>= fun e ->
      sim_substitute_aux zipped_exprs_ids (current_depth + 1) current_meta_depth
        current_type_depth e1
      >>= fun e1 ->
      sim_substitute_aux zipped_exprs_ids (current_depth + 1) current_meta_depth
        current_type_depth e2
      >>= fun e2 -> Ok (Ast.Expr.Case (e, iddef1, e1, iddef2, e2))
  | Ast.Expr.Lambda (iddef, e) ->
      sim_substitute_aux zipped_exprs_ids (current_depth + 1) current_meta_depth
        current_type_depth e
      >>= fun e -> Ok (Ast.Expr.Lambda (iddef, e))
  | Ast.Expr.Application (e1, e2) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e1
      >>= fun e1 ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e2
      >>= fun e2 -> Ok (Ast.Expr.Application (e1, e2))
  | Ast.Expr.IfThenElse (b, e1, e2) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth b
      >>= fun b ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e1
      >>= fun e1 ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e2
      >>= fun e2 -> Ok (Ast.Expr.IfThenElse (b, e1, e2))
  | Ast.Expr.LetBinding (iddef, e, e2) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e
      >>= fun e ->
      sim_substitute_aux zipped_exprs_ids (current_depth + 1) current_meta_depth
        current_type_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetBinding (iddef, e, e2))
  | Ast.Expr.LetRec (iddef, e, e2) ->
      sim_substitute_aux zipped_exprs_ids (current_depth + 1) current_meta_depth
        current_type_depth e
      >>= fun e ->
      sim_substitute_aux zipped_exprs_ids (current_depth + 1) current_meta_depth
        current_type_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetRec (iddef, e, e2))
  | Ast.Expr.LetRecMutual (iddef_e_list, e2) ->
      List.map iddef_e_list ~f:(fun (iddef, e) ->
          sim_substitute_aux zipped_exprs_ids (current_depth + 1)
            current_meta_depth current_type_depth e
          >>= fun e -> Ok (iddef, e))
      |> Or_error.combine_errors
      >>= fun iddef_e_list ->
      sim_substitute_aux zipped_exprs_ids (current_depth + 1) current_meta_depth
        current_type_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetRecMutual (iddef_e_list, e2))
  | Ast.Expr.Box (tvctx, ctx, e) -> Ok (Ast.Expr.Box (tvctx, ctx, e))
  | Ast.Expr.LetBox (metaid, e, e2) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e
      >>= fun e ->
      sim_substitute_aux zipped_exprs_ids current_depth (current_meta_depth + 1)
        current_type_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetBox (metaid, e, e2))
  | Ast.Expr.Closure (metaid, typs, exprs) ->
      exprs
      |> List.map
           ~f:
             (sim_substitute_aux zipped_exprs_ids current_depth
                current_meta_depth current_type_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Closure (metaid, typs, exprs))
  | Ast.Expr.Constr (tid, e_opt) -> (
      match e_opt with
      | None -> Ok (Ast.Expr.Constr (tid, e_opt))
      | Some e ->
          sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
            current_type_depth e
          >>= fun e -> Ok (Ast.Expr.Constr (tid, Some e)))
  | Ast.Expr.Match (e, pattn_expr_list) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e
      >>= fun e ->
      List.map pattn_expr_list ~f:(fun (pattn, expr) ->
          let binders = Ast.Pattern.get_binders pattn in
          let new_depth =
            if List.is_empty binders then current_depth else current_depth + 1
          in
          sim_substitute_aux zipped_exprs_ids new_depth current_meta_depth
            current_type_depth expr
          >>= fun expr -> Ok (pattn, expr))
      |> Or_error.combine_errors
      >>= fun pattn_expr_list -> Ok (Ast.Expr.Match (e, pattn_expr_list))
  | Ast.Expr.Lift (typ, expr) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth expr
      >>= fun expr -> Ok (Ast.Expr.Lift (typ, expr))
  | Ast.Expr.EValue v ->
      Ast.Value.to_expr v
      |> sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
           current_type_depth
  | Ast.Expr.Ref e ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e
      >>= fun e -> Ok (Ast.Expr.Ref e)
  | Ast.Expr.While (p, e) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth p
      >>= fun p ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e
      >>= fun e -> Ok (Ast.Expr.While (p, e))
  | Ast.Expr.Array exprs ->
      List.map exprs
        ~f:
          (sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
             current_type_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Array exprs)
  | Ast.Expr.ArrayAssign (e1, e2, e3) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e1
      >>= fun e1 ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e2
      >>= fun e2 ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e3
      >>= fun e3 -> Ok (Ast.Expr.ArrayAssign (e1, e2, e3))
  | Ast.Expr.BigLambda (v, e) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        (current_type_depth + 1) e
      >>= fun e -> Ok (Ast.Expr.BigLambda (v, e))
  | Ast.Expr.TypeApply (e, t) ->
      sim_substitute_aux zipped_exprs_ids current_depth current_meta_depth
        current_type_depth e
      >>= fun e -> Ok (Ast.Expr.TypeApply (e, t))

let sim_substitute_from_zipped_list expr_id_zipped expr_subst_in =
  sim_substitute_aux expr_id_zipped 0 0 0 expr_subst_in

let sim_substitute exprs context expr_subst_in =
  let open Or_error.Monad_infix in
  context
  |> List.map ~f:(fun (id, _) -> Ast.ObjIdentifier.get_name id)
  |> List.zip exprs
  |> fun zipped_or_uneq_length ->
  match zipped_or_uneq_length with
  | List.Or_unequal_lengths.Unequal_lengths ->
      error
        "SimultaneousSubstitutionError: Expr and Context have unequal lengths! \
         Type check must have gone wrong! (exprs, context, expr_subst_in)"
        (exprs, context, expr_subst_in)
        [%sexp_of: Ast.Expr.t list * Ast.Context.t * Ast.Expr.t]
  | List.Or_unequal_lengths.Ok zipped ->
      Ok zipped >>= fun zipped ->
      (*Assume that all the ids have De Bruijn index 0*)
      sim_substitute_aux zipped 0 0 0 expr_subst_in

let equal_meta_id_str_depth meta_id id_str de_bruijn_int =
  (*Checks whether the given object id corresponds to the id and the de bruijn index given*)
  de_bruijn_int |> Ast.DeBruijnIndex.create |> Or_error.ok
  |> Option.value ~default:Ast.DeBruijnIndex.none
  |> Ast.MetaIdentifier.of_string_and_index id_str
  |> Ast.MetaIdentifier.equal meta_id

(*
    Meta Substitution
    When we have [[[x, y, z]. e1/u]] (...u[0] with (a, b, c)...let box v = X in u[1] with (d, ... v[0]..., f))
    we want that
    - Every expression a, b, c be closed with the run time environment at (obj) depth 0 and metadepth 0
*)

let rec meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth
    expr_subst_in =
  let open Or_error.Monad_infix in
  match expr_subst_in with
  | Ast.Expr.Identifier oid -> Ok (Ast.Expr.Identifier oid)
  | Ast.Expr.Constant c -> Ok (Ast.Expr.Constant c)
  | Ast.Expr.UnaryOp (op, expr) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.UnaryOp (op, expr))
  | Ast.Expr.BinaryOp (op, expr, expr2) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
      >>= fun expr ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth
        expr2
      >>= fun expr2 -> Ok (Ast.Expr.BinaryOp (op, expr, expr2))
  | Ast.Expr.Prod exprs ->
      List.map exprs
        ~f:
          (meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Prod exprs)
  (* | Ast.Expr.Fst expr ->
         meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
         >>= fun expr -> Ok (Ast.Expr.Fst expr)
     | Ast.Expr.Snd expr ->
         meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
         >>= fun expr -> Ok (Ast.Expr.Snd expr) *)
  | Ast.Expr.Nth (expr, i) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.Nth (expr, i))
  | Ast.Expr.Left (t1, t2, expr) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.Left (t1, t2, expr))
  | Ast.Expr.Right (t1, t2, expr) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.Right (t1, t2, expr))
  | Ast.Expr.Case (e, iddef1, e1, iddef2, e2) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e1
      >>= fun e1 ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.Case (e, iddef1, e1, iddef2, e2))
  | Ast.Expr.Lambda (iddef, e) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.Lambda (iddef, e))
  | Ast.Expr.Application (e1, e2) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e1
      >>= fun e1 ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.Application (e1, e2))
  | Ast.Expr.IfThenElse (b, e1, e2) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth b
      >>= fun b ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e1
      >>= fun e1 ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.IfThenElse (b, e1, e2))
  | Ast.Expr.LetBinding (iddef, e, e2) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetBinding (iddef, e, e2))
  | Ast.Expr.LetRec (iddef, e, e2) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetRec (iddef, e, e2))
  | Ast.Expr.LetRecMutual (iddef_e_list, e2) ->
      List.map iddef_e_list ~f:(fun (iddef, e) ->
          meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth
            e
          >>= fun e -> Ok (iddef, e))
      |> Or_error.combine_errors
      >>= fun iddef_e_list ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e2
      >>= fun e2 -> Ok (Ast.Expr.LetRecMutual (iddef_e_list, e2))
  | Ast.Expr.Box (tvctx, ctx2, e) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.Box (tvctx, ctx2, e))
  | Ast.Expr.LetBox (metaid, e, e2) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      (*There was an error here. it's current_meta_depth without the +1 because u is non-binding in e*)
      >>=
      fun e ->
      meta_substitute_aux ctx expr_subst_for meta_id_str
        (current_meta_depth + 1) e2
      >>= fun e2 -> Ok (Ast.Expr.LetBox (metaid, e, e2))
  | Ast.Expr.Closure (metaid, typs, exprs) ->
      (* Debug code here
         let () =
           if String.equal (Ast.MetaIdentifier.get_name metaid) "res2_" then (
             print_endline
               (Printf.sprintf "Metaid subst for: %s, Meta Depth %i" meta_id_str
                  current_meta_depth);
             print_endline (Ast.MetaIdentifier.show metaid))
           else ()
         in *)
      exprs
      |> List.map
           ~f:
             (meta_substitute_aux ctx expr_subst_for meta_id_str
                current_meta_depth)
      |> Or_error.combine_errors
      |> Or_error.tag
           ~tag:
             "MetaSubstitutionError: Error meta-substituting on expressions in \
              the explicit substitution term."
      >>= fun exprs_in_subs ->
      (*Check for equality of u and u'*)
      if not (equal_meta_id_str_depth metaid meta_id_str current_meta_depth)
      then
        Ast.MetaIdentifier.shift ~depth:current_meta_depth ~offset:(-1) metaid
        |> fun or_error ->
        Or_error.tag_arg or_error
          "MetaSubstitutionError: Error in shifting (exprs_in_subs, metaid)"
          (exprs_in_subs, metaid)
          [%sexp_of: Ast.Expr.t list * Ast.MetaIdentifier.t]
        >>= fun metaid ->
        (*Debug
          let () =
            if String.equal (Ast.MetaIdentifier.get_name metaid) "res2_" then
              print_endline (Ast.MetaIdentifier.show metaid)
            else ()
          in *)
        Ok (Ast.Expr.Closure (metaid, typs, exprs_in_subs))
      else
        expr_subst_for
        |> Ast.Expr.shift_indices ~obj_depth:0 ~meta_depth:0 ~obj_offset:0
             ~type_depth:0 ~meta_offset:current_meta_depth ~type_offset:0
        (* TODO: Correct here to have proper type sub *)
        >>= fun expr_subst_for ->
        sim_substitute exprs_in_subs ctx expr_subst_for |> fun or_error ->
        Or_error.tag_arg or_error
          "MetaSubstitutionError: Problem in simulatenous substitution. (ctx, \
           expr_subst_for, meta_id_str, current_meta_depth)"
          (ctx, expr_subst_for, meta_id_str, current_meta_depth)
          [%sexp_of: Ast.Context.t * Ast.Expr.t * string * int]
  | Ast.Expr.Constr (tid, e_opt) -> (
      match e_opt with
      | None -> Ok (Ast.Expr.Constr (tid, e_opt))
      | Some e ->
          meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth
            e
          >>= fun e -> Ok (Ast.Expr.Constr (tid, Some e)))
  | Ast.Expr.Match (e, pattn_expr_list) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e ->
      List.map pattn_expr_list ~f:(fun (pattn, expr) ->
          meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth
            expr
          >>= fun expr -> Ok (pattn, expr))
      |> Or_error.combine_errors
      >>= fun pattn_expr_list -> Ok (Ast.Expr.Match (e, pattn_expr_list))
  | Ast.Expr.Lift (typ, expr) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
      >>= fun expr -> Ok (Ast.Expr.Lift (typ, expr))
  | Ast.Expr.EValue v ->
      Ast.Value.to_expr v
      |> meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth
  | Ast.Expr.Ref e ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.Ref e)
  | Ast.Expr.While (p, e) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth p
      >>= fun p ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.While (p, e))
  | Ast.Expr.Array exprs ->
      List.map exprs
        ~f:
          (meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Array exprs)
  | Ast.Expr.ArrayAssign (e1, e2, e3) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e1
      >>= fun e1 ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e2
      >>= fun e2 ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e3
      >>= fun e3 -> Ok (Ast.Expr.ArrayAssign (e1, e2, e3))
  | Ast.Expr.BigLambda (v, e) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.BigLambda (v, e))
  | Ast.Expr.TypeApply (e, t) ->
      meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth e
      >>= fun e -> Ok (Ast.Expr.TypeApply (e, t))

let meta_substitute ctx expr meta_id expr_subst_in =
  let meta_id_str = Ast.MetaIdentifier.get_name meta_id in
  let current_meta_depth = 0 in
  meta_substitute_aux ctx expr meta_id_str current_meta_depth expr_subst_in

let equal_typevar_str_depth obj_id id_str de_bruijn_int =
  (*Checks whether the given object id corresponds to the id and the de bruijn index given*)
  de_bruijn_int |> Ast.DeBruijnIndex.create |> Or_error.ok
  |> Option.value ~default:Ast.DeBruijnIndex.none
  |> Ast.TypeVar.of_string_and_index id_str
  |> Ast.TypeVar.equal obj_id

let rec type_sub_iddef (t_sub_for : Ast.Typ.t) (v : Ast.TypeVar.t)
    ~current_depth (id, t_sub_in) =
  let open Or_error.Monad_infix in
  type_type_substitute ~current_depth t_sub_for v t_sub_in >>= fun typ ->
  Ok (id, typ)

and type_type_substitute (t_sub_for : Ast.Typ.t) (v : Ast.TypeVar.t)
    ?(current_depth = 0) (t_sub_in : Ast.Typ.t) =
  let open Or_error.Monad_infix in
  (*[t1/v]t2*)
  (*Maybe I'll still need De Bruijn Indices*)
  match t_sub_in with
  | Ast.Typ.TUnit -> Ok Ast.Typ.TUnit
  | Ast.Typ.TBool -> Ok Ast.Typ.TBool
  | Ast.Typ.TInt -> Ok Ast.Typ.TInt
  | Ast.Typ.TChar -> Ok Ast.Typ.TChar
  | Ast.Typ.TString -> Ok Ast.Typ.TString
  | Ast.Typ.TIdentifier id ->
      (*
       TODO: This is to be changed for ADT polymorphism
    *)
      Ok (Ast.Typ.TIdentifier id)
  | Ast.Typ.TFun (t1, t2) ->
      type_type_substitute ~current_depth t_sub_for v t1 >>= fun t1 ->
      type_type_substitute ~current_depth t_sub_for v t2 >>= fun t2 ->
      Ok (Ast.Typ.TFun (t1, t2))
  | Ast.Typ.TBox (tvctx, ctx, t) ->
      let new_current_depth =
        if Ast.TypeVarContext.is_empty tvctx then current_depth
        else current_depth + 1
      in
      type_type_substitute ~current_depth:new_current_depth t_sub_for v t
      >>= fun t ->
      List.map
        ~f:(type_sub_iddef ~current_depth:new_current_depth t_sub_for v)
        ctx
      |> Or_error.combine_errors
      >>= fun ctx -> Ok (Ast.Typ.TBox (tvctx, ctx, t))
  | Ast.Typ.TProd tlist ->
      List.map ~f:(type_type_substitute ~current_depth t_sub_for v) tlist
      |> Or_error.combine_errors
      >>= fun tlist -> Ok (Ast.Typ.TProd tlist)
  | Ast.Typ.TSum (t1, t2) ->
      type_type_substitute ~current_depth t_sub_for v t1 >>= fun t1 ->
      type_type_substitute ~current_depth t_sub_for v t2 >>= fun t2 ->
      Ok (Ast.Typ.TSum (t1, t2))
  | Ast.Typ.TRef t ->
      type_type_substitute ~current_depth t_sub_for v t >>= fun t ->
      Ok (Ast.Typ.TRef t)
  | Ast.Typ.TArray t ->
      type_type_substitute ~current_depth t_sub_for v t >>= fun t ->
      Ok (Ast.Typ.TArray t)
  | Ast.Typ.TVar id ->
      (* Inspired from above substitutions *)
      if not (equal_typevar_str_depth id (Ast.TypeVar.get_name v) current_depth)
      then
        Ast.TypeVar.shift ~depth:current_depth ~offset:(-1) id
        |> fun or_error ->
        Or_error.tag_arg or_error
          "TypeSubstitutionError: Error in shifting (doing [t/'a]'b) ('a, 'b)"
          (v, id) [%sexp_of: Ast.TypeVar.t * Ast.TypeVar.t]
        >>= fun id -> Ok (Ast.Typ.TVar id)
      else
        t_sub_for
        |> Ast.Typ.shift_indices ~type_depth:0 ~type_offset:current_depth
        >>= fun t_sub_for -> Ok t_sub_for
  | Ast.Typ.TForall (id, typ) ->
      type_type_substitute ~current_depth:(current_depth + 1) t_sub_for v typ
      >>= fun typ -> Ok (Ast.Typ.TForall (id, typ))

and type_term_substitute t_sub_for v ?(current_depth = 0) expr_subst_in =
  let open Or_error.Monad_infix in
  match expr_subst_in with
  | Ast.Expr.Identifier oid -> Ok (Ast.Expr.Identifier oid)
  | Ast.Expr.Constant c -> Ok (Ast.Expr.Constant c)
  | Ast.Expr.UnaryOp (op, expr) ->
      type_term_substitute t_sub_for v expr ~current_depth >>= fun expr ->
      Ok (Ast.Expr.UnaryOp (op, expr))
  | Ast.Expr.BinaryOp (op, expr, expr2) ->
      type_term_substitute t_sub_for v expr ~current_depth >>= fun expr ->
      type_term_substitute t_sub_for v expr2 ~current_depth >>= fun expr2 ->
      Ok (Ast.Expr.BinaryOp (op, expr, expr2))
  | Ast.Expr.Prod exprs ->
      List.map exprs ~f:(type_term_substitute t_sub_for v ~current_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Prod exprs)
  (* | Ast.Expr.Fst expr ->
         meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
         >>= fun expr -> Ok (Ast.Expr.Fst expr)
     | Ast.Expr.Snd expr ->
         meta_substitute_aux ctx expr_subst_for meta_id_str current_meta_depth expr
         >>= fun expr -> Ok (Ast.Expr.Snd expr) *)
  | Ast.Expr.Nth (expr, i) ->
      type_term_substitute t_sub_for v ~current_depth expr >>= fun expr ->
      Ok (Ast.Expr.Nth (expr, i))
  | Ast.Expr.Left (t1, t2, expr) ->
      type_type_substitute t_sub_for v ~current_depth t1 >>= fun t1 ->
      type_type_substitute t_sub_for v ~current_depth t2 >>= fun t2 ->
      type_term_substitute t_sub_for v ~current_depth expr >>= fun expr ->
      Ok (Ast.Expr.Left (t1, t2, expr))
  | Ast.Expr.Right (t1, t2, expr) ->
      type_type_substitute t_sub_for v ~current_depth t1 >>= fun t1 ->
      type_type_substitute t_sub_for v ~current_depth t2 >>= fun t2 ->
      type_term_substitute t_sub_for v ~current_depth expr >>= fun expr ->
      Ok (Ast.Expr.Right (t1, t2, expr))
  | Ast.Expr.Case (e, iddef1, e1, iddef2, e2) ->
      type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
      type_sub_iddef t_sub_for v iddef1 ~current_depth >>= fun iddef1 ->
      type_term_substitute t_sub_for v ~current_depth e1 >>= fun e1 ->
      type_sub_iddef t_sub_for v iddef2 ~current_depth >>= fun iddef2 ->
      type_term_substitute t_sub_for v ~current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.Case (e, iddef1, e1, iddef2, e2))
  | Ast.Expr.Lambda (iddef, e) ->
      type_sub_iddef t_sub_for v iddef ~current_depth >>= fun iddef ->
      type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
      Ok (Ast.Expr.Lambda (iddef, e))
  | Ast.Expr.Application (e1, e2) ->
      type_term_substitute t_sub_for v ~current_depth e1 >>= fun e1 ->
      type_term_substitute t_sub_for v ~current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.Application (e1, e2))
  | Ast.Expr.IfThenElse (b, e1, e2) ->
      type_term_substitute t_sub_for v ~current_depth b >>= fun b ->
      type_term_substitute t_sub_for v ~current_depth e1 >>= fun e1 ->
      type_term_substitute t_sub_for v ~current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.IfThenElse (b, e1, e2))
  | Ast.Expr.LetBinding (iddef, e, e2) ->
      type_sub_iddef t_sub_for v iddef ~current_depth >>= fun iddef ->
      type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
      type_term_substitute t_sub_for v ~current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.LetBinding (iddef, e, e2))
  | Ast.Expr.LetRec (iddef, e, e2) ->
      type_sub_iddef t_sub_for v iddef ~current_depth >>= fun iddef ->
      type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
      type_term_substitute t_sub_for v ~current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.LetRec (iddef, e, e2))
  | Ast.Expr.LetRecMutual (iddef_e_list, e2) ->
      List.map iddef_e_list ~f:(fun (iddef, e) ->
          type_sub_iddef t_sub_for v iddef ~current_depth >>= fun iddef ->
          type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
          Ok (iddef, e))
      |> Or_error.combine_errors
      >>= fun iddef_e_list ->
      type_term_substitute t_sub_for v ~current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.LetRecMutual (iddef_e_list, e2))
  | Ast.Expr.Box (tvctx, ctx2, e) ->
      let new_current_depth =
        if Ast.TypeVarContext.is_empty tvctx then current_depth
        else current_depth + 1
      in
      List.map
        ~f:(type_sub_iddef ~current_depth:new_current_depth t_sub_for v)
        ctx2
      |> Or_error.combine_errors
      >>= fun ctx2 ->
      type_term_substitute t_sub_for v ~current_depth:new_current_depth e
      >>= fun e -> Ok (Ast.Expr.Box (tvctx, ctx2, e))
  | Ast.Expr.LetBox (metaid, e, e2) ->
      type_term_substitute t_sub_for v ~current_depth e
      (*There was an error here. it's current_meta_depth without the +1 because u is non-binding in e*)
      >>=
      fun e ->
      type_term_substitute t_sub_for v ~current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.LetBox (metaid, e, e2))
  | Ast.Expr.Closure (metaid, typs, exprs) ->
      List.map ~f:(type_type_substitute t_sub_for v ~current_depth) typs
      |> Or_error.combine_errors
      >>= fun typs ->
      exprs
      |> List.map ~f:(type_term_substitute t_sub_for v ~current_depth)
      |> Or_error.combine_errors
      |> Or_error.tag
           ~tag:
             "TypeSubstitutionError: Error type-substituting on expressions in \
              the explicit substitution term."
      >>= fun exprs_in_subs ->
      Ok (Ast.Expr.Closure (metaid, typs, exprs_in_subs))
  | Ast.Expr.Constr (tid, e_opt) -> (
      (*
       TODO: Modify for ADT Polymorphism
    *)
      match e_opt with
      | None -> Ok (Ast.Expr.Constr (tid, e_opt))
      | Some e ->
          type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
          Ok (Ast.Expr.Constr (tid, Some e)))
  | Ast.Expr.Match (e, pattn_expr_list) ->
      type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
      List.map pattn_expr_list ~f:(fun (pattn, expr) ->
          type_term_substitute t_sub_for v ~current_depth expr >>= fun expr ->
          Ok (pattn, expr))
      |> Or_error.combine_errors
      >>= fun pattn_expr_list -> Ok (Ast.Expr.Match (e, pattn_expr_list))
  | Ast.Expr.Lift (typ, expr) ->
      type_type_substitute t_sub_for v typ ~current_depth >>= fun typ ->
      type_term_substitute t_sub_for v ~current_depth expr >>= fun expr ->
      Ok (Ast.Expr.Lift (typ, expr))
  | Ast.Expr.EValue value ->
      Ast.Value.to_expr value |> type_term_substitute t_sub_for v ~current_depth
  | Ast.Expr.Ref e ->
      type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
      Ok (Ast.Expr.Ref e)
  | Ast.Expr.While (p, e) ->
      type_term_substitute t_sub_for v ~current_depth p >>= fun p ->
      type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
      Ok (Ast.Expr.While (p, e))
  | Ast.Expr.Array exprs ->
      List.map exprs ~f:(type_term_substitute t_sub_for v ~current_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Array exprs)
  | Ast.Expr.ArrayAssign (e1, e2, e3) ->
      type_term_substitute t_sub_for v ~current_depth e1 >>= fun e1 ->
      type_term_substitute t_sub_for v ~current_depth e2 >>= fun e2 ->
      type_term_substitute t_sub_for v ~current_depth e3 >>= fun e3 ->
      Ok (Ast.Expr.ArrayAssign (e1, e2, e3))
  | Ast.Expr.BigLambda (id, e) ->
      type_term_substitute t_sub_for v ~current_depth:(current_depth + 1) e
      >>= fun e -> Ok (Ast.Expr.BigLambda (id, e))
  | Ast.Expr.TypeApply (e, t) ->
      type_term_substitute t_sub_for v ~current_depth e >>= fun e ->
      type_type_substitute t_sub_for v t ~current_depth >>= fun t ->
      Ok (Ast.Expr.TypeApply (e, t))

let sim_type_type_substitute typs tvctx type_sub_in =
  let open Or_error.Monad_infix in
  List.fold2 tvctx typs ~init:(Ok type_sub_in) ~f:(fun typ_sub_in tv typ ->
    typ_sub_in >>= fun typ_sub_in -> type_type_substitute typ tv typ_sub_in)
  |> function
  | List.Or_unequal_lengths.Unequal_lengths ->
      error "SimultaneousSubtitutionError: Unequal Length typs and terms"
        (typs, tvctx) [%sexp_of: Ast.Typ.t list * Ast.TypeVarContext.t]
  | List.Or_unequal_lengths.Ok expr_subst_in -> expr_subst_in

let sim_type_term_substitute typs tvctx expr_sub_in =
    let open Or_error.Monad_infix in
    List.fold2 tvctx typs ~init:(Ok expr_sub_in) ~f:(fun e tv typ ->
      e >>= fun e -> type_term_substitute typ tv e)
    |> function
    | List.Or_unequal_lengths.Unequal_lengths ->
        error "SimultaneousSubtitutionError: Unequal Length typs and terms"
          (typs, tvctx) [%sexp_of: Ast.Typ.t list * Ast.TypeVarContext.t]
    | List.Or_unequal_lengths.Ok expr_subst_in -> expr_subst_in
  

let sim_substitute_with_types typs tvctx exprs context expr_subst_in =
  let open Or_error.Monad_infix in
  sim_type_term_substitute typs tvctx expr_subst_in >>= fun expr_subst_in ->
  sim_substitute exprs context expr_subst_in
