(* val type_check_program: Lys_ast.Past.Program.t -> Lys_ast.Ast.Program.t *)
(* val type_check_expression: Lys_ast.Ast.Typ.t -> Lys_ast.Ast.Expr.t -> unit
   val type_inference_expression: Lys_ast.Ast.Expr.t -> Lys_ast.Ast.Typ.t *)

open Lys_ast
open Core

val type_inference_expression :
  (Ast.TypeVarContext.t * Ast.Context.t * Ast.Typ.t * int)
  Typing_context.MetaTypingContext.t ->
  (Ast.Typ.t * int) Typing_context.ObjTypingContext.t ->
  Typing_context.TypeConstrContext.t ->
  int Typing_context.PolyTypeVarContext.t -> (* int = type_depth *)
  ?current_type_depth:int ->
  Ast.Expr.t ->
  Ast.Typ.t Or_error.t

val type_check_expression :
  (Ast.TypeVarContext.t * Ast.Context.t * Ast.Typ.t * int)
  Typing_context.MetaTypingContext.t ->
  (Ast.Typ.t * int) Typing_context.ObjTypingContext.t ->
  Typing_context.TypeConstrContext.t ->
  int Typing_context.PolyTypeVarContext.t ->
  ?current_type_depth:int ->
  Ast.Expr.t ->
  Ast.Typ.t ->
  unit Or_error.t

val type_check_program :
  ?meta_ctx:
    (Ast.TypeVarContext.t * Ast.Context.t * Ast.Typ.t * int)
    Typing_context.MetaTypingContext.t ->
  ?obj_ctx:(Ast.Typ.t * int) Typing_context.ObjTypingContext.t ->
  ?type_ctx:Typing_context.TypeConstrContext.t ->
  ?typevar_ctx:int Typing_context.PolyTypeVarContext.t ->
  Ast.Program.t ->
  Ast.TypedProgram.t Or_error.t
