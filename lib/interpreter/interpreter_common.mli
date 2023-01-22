open Lys_ast
open Lys_utils
open Core
open Lys_typing

module EvaluationContext : sig
  type single_record = {
    typ : Ast.Typ.t;
    rec_preface : (Ast.IdentifierDefn.t * Ast.Expr.t) list;
    value : Ast.Value.t;
  }
  [@@deriving sexp, show, compare, equal]

  type t = single_record String_map.t [@@deriving sexp, compare, equal]

  (*Map from object id to record (expr and is_rec)*)
  val set : t -> key:string -> data:single_record -> t
  val set_all : t -> (string * single_record) list -> t
  val find_or_error : t -> string -> single_record Or_error.t
  val empty : t
  val show : t -> string
  val to_typing_obj_context : t -> Ast.Typ.t Typing_context.ObjTypingContext.t
  val is_not_rec : single_record -> bool
  val is_single_rec : single_record -> bool
  val is_mut_rec : single_record -> bool
end

module type TypeConstrContext_type = sig
  type constr_record = {
    constr : Ast.Constructor.t;
    arg_type : Ast.Typ.t option;
    belongs_to_typ : Ast.TypeIdentifier.t;
  }
  [@@deriving sexp, show, equal, compare]

  type t [@@deriving sexp, equal, compare]

  val add_typ_from_decl :
    t ->
    Ast.TypeIdentifier.t * (Ast.Constructor.t * Ast.Typ.t option) list ->
    t Or_error.t (*Error thrown when duplicated constructor name*)

  val get_constr_from_typ :
    t -> Ast.TypeIdentifier.t -> constr_record list option
  (*None means type doesn't exist, Some [] means type exists but is empty*)

  val get_typ_from_constr : t -> Ast.Constructor.t -> constr_record option
  val empty : t

  val to_typing_decl :
    t ->
    (Ast.TypeIdentifier.t * (Ast.Constructor.t * Ast.Typ.t option) list) list

  val to_typeconstrtypingcontext : t -> Typing_context.TypeConstrTypingContext.t
  val show : t -> string
end

module TypeConstrContext : TypeConstrContext_type

module TopLevelEvaluationResult : sig
  type verbose = { steps : Ast.Expr.t list }
  [@@deriving sexp, compare, equal, show]

  type t =
    | ExprValue of
        Ast.Typ.t * Ast.Value.t * float option * int option * verbose option
    | Defn of
        Ast.IdentifierDefn.t
        * Ast.Value.t
        * float option
        * int option
        * verbose option
    | RecDefn of
        Ast.IdentifierDefn.t
        * Ast.Value.t
        * float option
        * int option
        * verbose option
    | MutRecDefn of
        (Ast.IdentifierDefn.t
        * Ast.Value.t
        * float option
        * int option
        * verbose option)
        list
    | Directive of Ast.Directive.t * string
    | DatatypeDecl of
        Ast.TypeIdentifier.t * (Ast.Constructor.t * Ast.Typ.t option) list
  [@@deriving sexp, compare, equal, show]

  val get_str_output : t -> string
end
