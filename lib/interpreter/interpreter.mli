open Lys_ast
open Lys_utils

module EvaluationContext : sig
  type single_record = { typ:Ast.Typ.t; is_rec : bool; expr : Ast.Expr.t } [@@deriving sexp, compare, equal]
  type t = single_record String_map.t [@@deriving sexp, compare, equal] (*Map from object id to record (expr and is_rec)*)
end

(* val reduce : (* Single step *)
  top_level_context:EvaluationContext.t -> expr:Ast.Expr.t -> Ast.Expr.t *)

val multi_step_reduce : (* Glue multiple steps *)
  top_level_context:EvaluationContext.t -> expr:Ast.Expr.t -> Ast.Expr.t

(* val evaluate : (* Independent, big step *)
  top_level_context:EvaluationContext.t -> expr:Ast.Expr.t -> Ast.Expr.t *)

val evaluate_program: Ast.Program.t -> string list
