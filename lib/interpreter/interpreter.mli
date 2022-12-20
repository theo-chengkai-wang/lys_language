open Core
open Lys_ast

module EvaluationContext : sig
  type single_record = { obj_id : string; is_rec : bool; expr : Ast.Expr.t }
  type t = single_record list [@@deriving sexp, show, compare, equal]
end

val reduce : (* Single step *)
  top_level_context:EvaluationContext.t -> expr:Ast.Expr.t -> Ast.Expr.t

val multi_step_reduce : (* Glue multiple steps *)
  top_level_context:EvaluationContext.t -> expr:Ast.Expr.t -> Ast.Expr.t

val evaluate : (* Independent, big step *)
  top_level_context:EvaluationContext.t -> expr:Ast.Expr.t -> Ast.Expr.t

val evaluate_program: Ast.Program.t -> string list
