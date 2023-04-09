open Lys_utils
open Lys_ast
open Core

module type TypingContext_type = sig
  module Key : sig
    type t [@@deriving sexp, equal, compare]

    val get_name : t -> string
    val of_string : string -> t
  end

  type 'b t [@@deriving sexp, equal, compare]

  val create_empty_context : unit -> 'b t
  val add_mapping : 'b t -> Key.t -> 'b -> 'b t

  (* val delete_last_mapping : 'b t -> Key.t -> 'b t *)
  val get_last_mapping : 'b t -> Key.t -> 'b option
  val add_all_mappings : 'b t -> (Key.t * 'b) list -> 'b t
  val is_in_context : 'b t -> Key.t -> bool
  val get_all_mappings_as_list : 'b t -> (Key.t * 'b) list
end

module ObjTypingContext : TypingContext_type with module Key = Ast.ObjIdentifier

module MetaTypingContext :
  TypingContext_type with module Key = Ast.MetaIdentifier

module PolyTypeVarContext : TypingContext_type with module Key = Ast.TypeVar
[@@deriving show]

module type TypeConstrTypingContext_type = sig
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
end

module TypeConstrTypingContext : TypeConstrTypingContext_type
