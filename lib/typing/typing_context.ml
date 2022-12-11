open Lys_utils
open Lys_ast

module ObjTypingContext :
  Context.Context_S_type with type key = Ast.ObjIdentifier.t =
  Context.NaiveContext.Make (Ast.ObjIdentifier)

module MetaTypingContext :
  Context.Context_S_type with type key = Ast.MetaIdentifier.t =
  Context.NaiveContext.Make (Ast.MetaIdentifier)

module TypeTypingContext :
  Context.Context_S_type with type key = Ast.TypeIdentifier.t =
  Context.NaiveContext.Make (Ast.TypeIdentifier)
