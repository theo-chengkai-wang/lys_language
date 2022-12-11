open Lys_ast
open Core

(*
  TODO: Maybe instead of doing 1 pass, do 2 passes to distinguish Past->Ast and Type checking AST.   
*)

(* meta_ctx->ctx->Past.Expr.t -> Past.Typ.t -> Ast.Expr.t * Ast.Typ.t *)
let type_check_expression meta_ctx ctx past_expr past_typ =
  (Ast.Expr.Constant Ast.Constant.Unit, Ast.Typ.TUnit)

(* meta_ctx -> ctx -> Past.Expr.t -> (Ast.Expr.t * Ast.Typ.t) *)
let type_inference_expression meta_ctx ctx past_expr =
  (Ast.Expr.Constant Ast.Constant.Unit, Ast.Typ.TUnit)

(* meta_ctx -> ctx -> (Past.Expr.t) -> Past.Typ.t -> (Ast.Expr.t*Ast.Typ.t* MetaContext.t *Context.t)*)
let process_decl meta_ctx ctx past_expr past_typ =
  ( Ast.Expr.Constant Ast.Constant.Unit,
    Ast.Typ.TUnit,
    Typing_context.ObjTypingContext.create_empty_context,
    Typing_context.MetaTypingContext.create_empty_context )

(* meta_ctx->ctx->Past.program->Ast.Program *)
let type_check_program_aux meta_ctx ctx program =
  match  program with
  | 
  | (Ast.TopLevelDefn.Definition)::program

let type_check_program program =
  type_check_program_aux NaiveTypingContext.empty_context
    NaiveTypingContext.empty_context program
