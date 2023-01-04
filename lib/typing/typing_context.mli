open Lys_utils
open Lys_ast
open Core

module ObjTypingContext :
  Context.NaiveContext.S with type Key.t = Ast.ObjIdentifier.t

module MetaTypingContext :
  Context.NaiveContext.S with type Key.t = Ast.MetaIdentifier.t

module type TypeConstrTypingContext_type = sig
  type constr_record = {
    constr : Ast.Constructor.t;
    arg_type : Ast.Typ.t;
    belongs_to_typ : Ast.TypeIdentifier.t;
  }
  [@@deriving sexp, show, equal, compare]

  type t  [@@deriving sexp, equal, compare]

  val add_typ_from_decl :
    t ->
    Ast.TypeIdentifier.t * (Ast.Constructor.t * Ast.Typ.t) list ->
    t Or_error.t (*Error thrown when duplicated constructor name*)

  val get_constr_from_typ :
    t -> Ast.TypeIdentifier.t -> constr_record list option
  (*None means type doesn't exist, Some [] means type exists but is empty*)

  val get_typ_from_constr : t -> Ast.Constructor.t -> constr_record option
  val empty: t
end

module TypeConstrTypingContext: TypeConstrTypingContext_type
