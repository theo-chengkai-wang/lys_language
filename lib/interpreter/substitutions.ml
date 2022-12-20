open Core
open Lys_ast

(*
   This definition takes an id (string) and a depth (of the tree) with depth 0 at root. We recurse down the tree adding depth
    when going through a binder, and shift the tree BY to the current tree depth when substituting in. In other words,
    we assume that in [e/z]e', e is closed at depth 0 (so any free variable it contains have De Bruijn indices with depth 0).
*)

let equal_id_str_depth obj_id id_str de_bruijn_int =
  (*Checks whether the given object id corresponds to the id and the de bruijn index given*)
  de_bruijn_int |> Ast.DeBruijnIndex.create
  |> Ast.ObjIdentifier.of_string_and_index id_str
  |> Ast.ObjIdentifier.equal obj_id

let rec substitute_aux expr_subst_for id_str current_depth expr_subst_in =
  let open Or_error.Monad_infix in
  match expr_subst_in with
  | Ast.Expr.Identifier oid ->
      if not (equal_id_str_depth oid id_str current_depth) then
        Ok (Ast.Expr.Identifier oid)
      else
        Ast.Expr.shift_indices expr_subst_for ~obj_offset:current_depth
          ~meta_offset:0
        |> Or_error.tag
             ~tag:
               (Printf.sprintf
                  "SubstitutionError: Index shifting failed when substituting \
                   expr %s for %s"
                  (Ast.Expr.show expr_subst_for)
                  id_str)
  | Ast.Expr.Constant c -> Ok (Ast.Expr.Constant c)
  | Ast.Expr.UnaryOp (op, expr) ->
      substitute_aux expr_subst_for id_str current_depth expr >>= fun expr ->
      Ok (Ast.Expr.UnaryOp (op, expr))
  | Ast.Expr.BinaryOp (op, expr, expr2) ->
      substitute_aux expr_subst_for id_str current_depth expr >>= fun expr ->
      substitute_aux expr_subst_for id_str current_depth expr2 >>= fun expr2 ->
      Ok (Ast.Expr.BinaryOp (op, expr, expr2))
  | Ast.Expr.Prod (expr1, expr2) ->
      substitute_aux expr_subst_for id_str current_depth expr1 >>= fun expr1 ->
      substitute_aux expr_subst_for id_str current_depth expr2 >>= fun expr2 ->
      Ok (Ast.Expr.Prod (expr1, expr2))
  | Ast.Expr.Fst expr ->
      substitute_aux expr_subst_for id_str current_depth expr >>= fun expr ->
      Ok (Ast.Expr.Fst expr)
  | Ast.Expr.Snd expr ->
      substitute_aux expr_subst_for id_str current_depth expr >>= fun expr ->
      Ok (Ast.Expr.Snd expr)
  | Ast.Expr.Left (t1, t2, expr) ->
      substitute_aux expr_subst_for id_str current_depth expr >>= fun expr ->
      Ok (Ast.Expr.Left (t1, t2, expr))
  | Ast.Expr.Right (t1, t2, expr) ->
      substitute_aux expr_subst_for id_str current_depth expr >>= fun expr ->
      Ok (Ast.Expr.Right (t1, t2, expr))
  | Ast.Expr.Match (e, iddef1, e1, iddef2, e2) ->
      substitute_aux expr_subst_for id_str current_depth e >>= fun e ->
      substitute_aux expr_subst_for id_str (current_depth + 1) e1 >>= fun e1 ->
      substitute_aux expr_subst_for id_str (current_depth + 1) e2 >>= fun e2 ->
      Ok (Ast.Expr.Match (e, iddef1, e1, iddef2, e2))
  | Ast.Expr.Lambda (iddef, e) ->
      substitute_aux expr_subst_for id_str (current_depth + 1) e >>= fun e ->
      Ok (Ast.Expr.Lambda (iddef, e))
  | Ast.Expr.Application (e1, e2) ->
      substitute_aux expr_subst_for id_str current_depth e1 >>= fun e1 ->
      substitute_aux expr_subst_for id_str current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.Application (e1, e2))
  | Ast.Expr.IfThenElse (b, e1, e2) ->
      substitute_aux expr_subst_for id_str current_depth b >>= fun b ->
      substitute_aux expr_subst_for id_str current_depth e1 >>= fun e1 ->
      substitute_aux expr_subst_for id_str current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.IfThenElse (b, e1, e2))
  | Ast.Expr.LetBinding (iddef, e, e2) ->
      substitute_aux expr_subst_for id_str current_depth e >>= fun e ->
      substitute_aux expr_subst_for id_str (current_depth + 1) e2 >>= fun e2 ->
      Ok (Ast.Expr.LetBinding (iddef, e, e2))
  | Ast.Expr.LetRec (iddef, e, e2) ->
      substitute_aux expr_subst_for id_str (current_depth + 1) e >>= fun e ->
      substitute_aux expr_subst_for id_str (current_depth + 1) e2 >>= fun e2 ->
      Ok (Ast.Expr.LetRec (iddef, e, e2))
  | Ast.Expr.Box (ctx, e) -> Ok (Ast.Expr.Box (ctx, e))
  | Ast.Expr.LetBox (metaid, e, e2) ->
      substitute_aux expr_subst_for id_str current_depth e >>= fun e ->
      substitute_aux expr_subst_for id_str current_depth e2 >>= fun e2 ->
      Ok (Ast.Expr.LetBox (metaid, e, e2))
  | Ast.Expr.Closure (metaid, exprs) ->
      exprs
      |> List.map ~f:(substitute_aux expr_subst_for id_str current_depth)
      |> Or_error.combine_errors
      >>= fun exprs -> Ok (Ast.Expr.Closure (metaid, exprs))

let substitute expr_subst_for id expr_subst_in = 
  let id_str = Ast.ObjIdentifier.get_name id in
  let current_depth = 0 in
  substitute_aux expr_subst_for id_str current_depth expr_subst_in

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
