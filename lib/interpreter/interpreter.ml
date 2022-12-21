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
open Lys_ast
open Core
open Lys_utils

module EvaluationContext : sig
  type single_record = { typ : Ast.Typ.t; is_rec : bool; expr : Ast.Expr.t }
  [@@deriving sexp, compare, equal]

  type t = single_record String_map.t [@@deriving sexp, compare, equal]

  val set : t -> key:string -> data:single_record -> t
  val find_or_error : t -> string -> single_record Or_error.t
end = struct
  type single_record = { typ : Ast.Typ.t; is_rec : bool; expr : Ast.Expr.t }
  [@@deriving sexp, compare, equal]

  type t = single_record String_map.t [@@deriving sexp, compare, equal]

  let set = String_map.set

  let find_or_error map key =
    match String_map.find map key with
    | Some v -> Ok v
    | None -> error "StringMapError: Key not found." key [%sexp_of: string]
end
(*
   let reduce ~top_level_context ~expr = ()

   let evaluate_program program = [] *)

let rec multi_step_reduce ~top_level_context ~expr =
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
        >>= fun { typ; is_rec; expr } ->
        if not is_rec then
          (*Substitution -- no need to worry about De Bruijn indices as there is no way they can go wrong*)
          Ok expr
        else
          Ast.DeBruijnIndex.create 0 >>= fun debruijn_index ->
          Ok
            (Ast.Expr.LetRec
               ( (Ast.ObjIdentifier.of_string id_str, typ),
                 expr,
                 Ast.Expr.Identifier
                   (Ast.ObjIdentifier.of_string_and_index id_str debruijn_index)
               ))
  | Ast.Expr.Constant c -> Ok (expr)
  | Ast.Expr.UnaryOp (op, expr) -> 
    multi_step_reduce ~top_level_context ~expr >>= fun expr -> Ok ()
      
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
