open Core
open Lys_ast

(*
  TODO: Maybe instead of doing 1 pass, do 2 passes to distinguish Past->Ast and Type checking AST.   
*)

(* meta_ctx->ctx->Past.Expr.t -> Past.Typ.t -> Ast.Expr.t * Ast.Typ.t *)
let type_check_expression _ _ _ _ = ()

(* meta_ctx -> ctx -> Past.Expr.t -> (Ast.Expr.t * Ast.Typ.t) *)
let type_inference_expression _ _ _ = Ast.Typ.TUnit

(* meta_ctx -> ctx -> (Past.Expr.t) -> Past.Typ.t -> (Ast.Expr.t*Ast.Typ.t* MetaContext.t *Context.t)*)
(* let process_decl _ _ _ =
  ( Ast.Expr.Constant Ast.Constant.Unit,
    Ast.Typ.TUnit,
    Typing_context.ObjTypingContext.create_empty_context (),
    Typing_context.MetaTypingContext.create_empty_context () ) *)

(* meta_ctx->ctx->Past.program->Ast.Program *)
(* let rec type_check_program_aux meta_ctx ctx program =
   match program with
   | Past.TopLevelDefn.Directive d :: program ->
       let new_ctx =
         if Past.Directive.equal d Past.Directive.Reset then
           Typing_context.ObjTypingContext.create_empty_context ()
         else ctx
       in
       Past.TopLevelDefn.Directive d
       :: type_check_program_aux meta_ctx new_ctx program
   | Past.TopLevelDefn.Definition () :: program ->
*)

let process_top_level meta_ctx ctx = function
  | Ast.TopLevelDefn.Definition (iddef, e) ->
      let id, typ = iddef in
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      let () = type_check_expression meta_ctx ctx e typ in
      (Ast.TypedTopLevelDefn.Definition (typ, iddef, e), meta_ctx, new_ctx)
  | Ast.TopLevelDefn.RecursiveDefinition (iddef, e) ->
      let id, typ = iddef in
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      let () = type_check_expression meta_ctx new_ctx e typ in
      (*Note that here we type check in the new context (self reference)*)
      (Ast.TypedTopLevelDefn.Definition (typ, iddef, e), meta_ctx, new_ctx)
  | Ast.TopLevelDefn.Directive d ->
      let new_ctx =
        if Ast.Directive.equal d Ast.Directive.Reset then
          Typing_context.ObjTypingContext.create_empty_context ()
        else ctx
      in
      (Ast.TypedTopLevelDefn.Directive d, meta_ctx, new_ctx)
  | Ast.TopLevelDefn.Expression e ->
      ( Ast.TypedTopLevelDefn.Expression
          (type_inference_expression meta_ctx ctx e, e),
        meta_ctx,
        ctx )

let rec type_check_program_aux meta_ctx ctx program =
  match program with
  | [] -> []
  | top :: tops ->
      let typed_top, new_meta, new_ctx = process_top_level meta_ctx ctx top in
      typed_top :: type_check_program_aux new_meta new_ctx tops

let type_check_program program =
  program |> Ast.Program.of_past
  |> type_check_program_aux
       (Typing_context.MetaTypingContext.create_empty_context ())
       (Typing_context.ObjTypingContext.create_empty_context ())
