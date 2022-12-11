open Lys_utils
open Lys_ast

module ObjTypingContext :
  Context.Context_S_type with type key = Ast.ObjIdentifier.t

module MetaTypingContext :
  Context.Context_S_type with type key = Ast.MetaIdentifier.t

module TypeTypingContext :
  Context.Context_S_type with type key = Ast.TypeIdentifier.t
