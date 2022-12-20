open Lys_ast

val substitute: Ast.Expr.t -> Ast.ObjIdentifier.t -> Ast.Expr.t -> Ast.Expr.t
(* [e/z]e' *)

val sim_substitute: Ast.Expr.t list -> Ast.Context.t -> Ast.Expr.t -> Ast.Expr.t
(* [\sigma / \Psi] e *)

val meta_substitute: Ast.Context.t -> Ast.Expr.t -> Ast.MetaIdentifier.t -> Ast.Expr.t
(* [\Psi.e/u]e' *)
