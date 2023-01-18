open Lys_ast
open Core
open Interpreter_common

type t =
  | MultiStep of { show_time : bool }
  (*Show Time and Show Steps*)
  | SingleStep of { show_step_count : bool; verbose : bool }
(*Show Steps and Verbose*)
[@@deriving show, sexp]

val arg_type: t Command.Arg_type.t

val evaluate_top_level_defns :
  ?top_level_context:EvaluationContext.t ->
  ?type_constr_context:TypeConstrContext.t ->
  ?interpreter:t ->
  Ast.TypedProgram.t ->
  (TopLevelEvaluationResult.t list * EvaluationContext.t * TypeConstrContext.t)
  Or_error.t

val evaluate_program :
  ?top_level_context:EvaluationContext.t ->
  ?type_constr_context:TypeConstrContext.t ->
  ?interpreter:t ->
  Ast.TypedProgram.t ->
  TopLevelEvaluationResult.t list Or_error.t
