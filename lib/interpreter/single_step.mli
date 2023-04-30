open Lys_ast
open Core
open Interpreter_common

module ReduceResult : sig
  type t =
    | NotReduced of Ast.Value.t
    | ReducedToVal of Ast.Value.t
    | ReducedToExpr of Ast.Expr.t
  [@@deriving sexp, show, equal, compare]

  val process :
    reduced:(Ast.Expr.t -> 'a) ->
    ?reduced_to_val:(Ast.Value.t -> 'a) ->
    not_reduced:(Ast.Value.t -> 'a) ->
    t ->
    'a
end

module TypeCheckEachStep : sig
  type t =
    | NoCheck
    | CheckType of Ast.Typ.t
    | CheckRec of Ast.IdentifierDefn.t list * Ast.Typ.t
end


val reduce :
  top_level_context:EvaluationContext.t ->
  type_constr_context:TypeConstrContext.t ->
  Ast.Expr.t ->
  ReduceResult.t Or_error.t

val multi_step_reduce :
  top_level_context:EvaluationContext.t ->
  type_constr_context:TypeConstrContext.t ->
  ?verbose:bool ->
  ?type_to_check:TypeCheckEachStep.t ->
  Ast.Expr.t ->
  (Ast.Value.t * int * Ast.Expr.t list option) Or_error.t

val evaluate_top_level_defn :
  ?top_level_context:EvaluationContext.t ->
  ?type_constr_context:TypeConstrContext.t ->
  ?show_step_count:bool ->
  ?verbose:bool ->
  ?type_check_each_step:bool ->
  Ast.TypedTopLevelDefn.t ->
  (TopLevelEvaluationResult.t * EvaluationContext.t * TypeConstrContext.t)
  Base.Or_error.t
