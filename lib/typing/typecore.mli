(* val type_check_program: Lys_ast.Past.Program.t -> Lys_ast.Ast.Program.t *)
(* val type_check_expression: Lys_ast.Ast.Typ.t -> Lys_ast.Ast.Expr.t -> unit
   val type_inference_expression: Lys_ast.Ast.Expr.t -> Lys_ast.Ast.Typ.t *)

open Lys_ast
open Core

val type_inference_expression :
  (Ast.Context.t * Ast.Typ.t) Typing_context.MetaTypingContext.t ->
  Ast.Typ.t Typing_context.ObjTypingContext.t ->
  Ast.Expr.t ->
  Ast.Typ.t Or_error.t

val type_check_expression :
  (Ast.Context.t * Ast.Typ.t) Typing_context.MetaTypingContext.t ->
  Ast.Typ.t Typing_context.ObjTypingContext.t ->
  Ast.Expr.t ->
  Ast.Typ.t ->
  unit Or_error.t

val type_check_program :
  ?meta_ctx:(Ast.Context.t * Ast.Typ.t) Typing_context.MetaTypingContext.t ->
  ?obj_ctx:Ast.Typ.t Typing_context.ObjTypingContext.t ->
  Ast.Program.t ->
  Ast.TypedProgram.t Or_error.t
