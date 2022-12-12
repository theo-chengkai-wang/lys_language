(* val type_check_program: Lys_ast.Past.Program.t -> Lys_ast.Ast.Program.t *)
(* val type_check_expression: Lys_ast.Ast.Typ.t -> Lys_ast.Ast.Expr.t -> unit
val type_inference_expression: Lys_ast.Ast.Expr.t -> Lys_ast.Ast.Typ.t *)

open Lys_ast

val type_check_program: Past.Program.t -> Ast.TypedProgram.t
