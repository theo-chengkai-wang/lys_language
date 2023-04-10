open Lys_ast
open Core

val substitute :
  Ast.Expr.t -> Ast.ObjIdentifier.t -> Ast.Expr.t -> Ast.Expr.t Or_error.t
(* [e/z]e' *)

val sim_substitute_from_zipped_list :
  (Ast.Expr.t * string) list -> Ast.Expr.t -> Ast.Expr.t Or_error.t

val sim_substitute :
  Ast.Expr.t list -> Ast.Context.t -> Ast.Expr.t -> Ast.Expr.t Or_error.t
(* [\sigma / \Psi] e *)

val meta_substitute :
  Ast.Context.t ->
  Ast.Expr.t ->
  Ast.MetaIdentifier.t ->
  Ast.Expr.t ->
  Ast.Expr.t Or_error.t
(* [\Psi.e/u]e' *)

val type_type_substitute :
  Ast.Typ.t ->
  Ast.TypeVar.t ->
  ?current_depth:int ->
  Ast.Typ.t ->
  Ast.Typ.t Or_error.t

val type_term_substitute :
  Ast.Typ.t ->
  Ast.TypeVar.t ->
  ?current_depth:int ->
  Ast.Expr.t ->
  Ast.Expr.t Or_error.t

val sim_type_type_substitute: Ast.Typ.t list -> Ast.TypeVarContext.t -> Ast.Typ.t -> Ast.Typ.t Or_error.t

val sim_type_term_substitute :
  Ast.Typ.t list -> Ast.TypeVarContext.t -> Ast.Expr.t -> Ast.Expr.t Or_error.t

val sim_substitute_with_types :
  Ast.Typ.t list ->
  Ast.TypeVarContext.t ->
  Ast.Expr.t list ->
  Ast.Context.t ->
  Ast.Expr.t ->
  Ast.Expr.t Or_error.t
