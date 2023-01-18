open Lys_ast
open Lys_utils
open Core
open Lys_typing
open Interpreter_common
(* val reduce : (* Single step *)
   top_level_context:EvaluationContext.t -> expr:Ast.Expr.t -> Ast.Expr.t *)

(* Glue multiple steps *)
val multi_step_reduce_with_step_count :
  top_level_context:EvaluationContext.t ->
  type_constr_context:TypeConstrContext.t ->
  expr:Ast.Expr.t ->
  (Ast.Value.t * int) Or_error.t

val evaluate_top_level_defn_with_step_count :
  ?top_level_context:EvaluationContext.t ->
  ?type_constr_context:TypeConstrContext.t ->
  ?time_exec:bool ->
  ?show_reduction_steps:bool ->
  Ast.TypedTopLevelDefn.t ->
  (TopLevelEvaluationResult.t * EvaluationContext.t * TypeConstrContext.t)
  Base.Or_error.t

val multi_step_reduce :
  top_level_context:EvaluationContext.t ->
  type_constr_context:TypeConstrContext.t ->
  expr:Ast.Expr.t ->
  Ast.Value.t Or_error.t

(* val evaluate : (* Independent, big step *)
   top_level_context:EvaluationContext.t -> expr:Ast.Expr.t -> Ast.Expr.t *)
val evaluate_top_level_defn :
  ?top_level_context:EvaluationContext.t ->
  ?type_constr_context:TypeConstrContext.t ->
  ?time_exec:bool ->
  Ast.TypedTopLevelDefn.t ->
  (TopLevelEvaluationResult.t * EvaluationContext.t * TypeConstrContext.t)
  Base.Or_error.t
