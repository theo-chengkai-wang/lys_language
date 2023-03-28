open Core
open Lys_utils

module DeBruijnIndex : sig
  (*Implementation of De Bruijn Indices -- encapsulated*)
  type t [@@deriving sexp, show, compare, equal]

  val none : t
  val top_level : t
  val create : int -> t Or_error.t
  val shift : t -> int -> int -> t Or_error.t
  val value : t -> default:int -> int
end

module type ObjIdentifier_type = sig
  type t [@@deriving sexp, show, compare, equal]

  val of_string : string -> t
  val of_past : Past.Identifier.t -> t
  val of_string_and_index : string -> DeBruijnIndex.t -> t
  val get_name : t -> string
  val get_debruijn_index : t -> DeBruijnIndex.t

  val populate_index :
    t ->
    current_ast_level:int ->
    current_identifiers:int String_map.t ->
    current_meta_ast_level:int ->
    current_meta_identifiers:int String_map.t ->
    t Or_error.t

  val shift : t -> depth:int -> offset:int -> t Or_error.t
end

module type Constructor_type = sig
  type t [@@deriving sexp, show, compare, equal]

  val of_string : string -> t
  val of_past : Past.Constructor.t -> t
  val get_name : t -> string
end

module type TypeIdentifier_type = sig
  (*Unused for now*)
  type t [@@deriving sexp, show, compare, equal]

  val get_name : t -> string
  val of_string : string -> t
  val of_past : Past.Identifier.t -> t
end

module type MetaIdentifier_type = sig
  type t [@@deriving sexp, show, compare, equal]

  val of_string : string -> t
  val of_past : Past.Identifier.t -> t
  val of_string_and_index : string -> DeBruijnIndex.t -> t
  val get_name : t -> string
  val get_debruijn_index : t -> DeBruijnIndex.t

  val populate_index :
    t ->
    current_ast_level:int ->
    current_identifiers:int String_map.t ->
    current_meta_ast_level:int ->
    current_meta_identifiers:int String_map.t ->
    t Or_error.t

  val shift : t -> depth:int -> offset:int -> t Or_error.t
end

module rec ObjIdentifier : ObjIdentifier_type
and MetaIdentifier : MetaIdentifier_type
and TypeIdentifier : TypeIdentifier_type
and Constructor : Constructor_type

and Typ : sig
  type t =
    | TUnit
    | TBool
    | TInt
    | TChar
    | TString
    | TIdentifier of TypeIdentifier.t
    | TFun of t * t
    | TBox of Context.t * t
    | TProd of t list
    | TSum of t * t
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.Typ.t -> t
end

and IdentifierDefn : sig
  type t = ObjIdentifier.t * Typ.t [@@deriving sexp, show, compare, equal]

  val of_past : Past.IdentifierDefn.t -> t
end

and Context : sig
  type t = IdentifierDefn.t list [@@deriving sexp, show, compare, equal]

  val of_past : Past.Context.t -> t
end

and BinaryOperator : sig
  type t =
    | ADD
    | SUB
    | MUL
    | DIV
    | MOD
    | EQ
    | NEQ
    | GTE
    | GT
    | LTE
    | LT
    | AND
    | OR
    | CHARSTRINGCONCAT
    | STRINGCONCAT
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.BinaryOperator.t -> t
end

and UnaryOperator : sig
  type t = NEG | NOT [@@deriving sexp, show, compare, equal]

  val of_past : Past.UnaryOperator.t -> t
end

and Constant : sig
  type t =
    | Integer of int
    | Boolean of bool
    | Unit
    | Character of char
    | String of string
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.Constant.t -> t
end

and Pattern : sig
  type t =
    | Datatype of (Constructor.t * ObjIdentifier.t list)
      (*Empty list means that data type doesn't have arguments.*)
    | Inl of ObjIdentifier.t
    | Inr of ObjIdentifier.t
    | Prod of ObjIdentifier.t list
    | Id of ObjIdentifier.t
    | Wildcard
    | String of string
    | ConcatCharString of ObjIdentifier.t * ObjIdentifier.t
  [@@deriving sexp, show, equal, compare]

  val of_past : Past.Pattern.t -> t
  val get_binders : t -> ObjIdentifier.t list
end

and Expr : sig
  type t =
    | Identifier of ObjIdentifier.t (*x*)
    | Constant of Constant.t (*c*)
    | UnaryOp of UnaryOperator.t * t (*unop e*)
    | BinaryOp of BinaryOperator.t * t * t (*e op e'*)
    | Prod of t list (*(e, e')*)
    (* | Fst of t (*fst e*)
       | Snd of t snd e *)
    | Nth of (t * int)
    | Left of Typ.t * Typ.t * t (*L[A,B] e*)
    | Right of Typ.t * Typ.t * t (*R[A,B] e*)
    | Case of t * IdentifierDefn.t * t * IdentifierDefn.t * t
      (*case e of
        L (x: A) -> e' | R (y: B) -> e'' translates to 1 expr and 2 lambdas*)
    | Lambda of IdentifierDefn.t * t (*fun (x : A) -> e*)
    | Application of t * t (*e e'*)
    | IfThenElse of t * t * t (*if e then e' else e''*)
    | LetBinding of IdentifierDefn.t * t * t (*let x: A = e in e'*)
    | LetRec of IdentifierDefn.t * t * t
    (*let rec f: A->B =
      e[f] in e'*)
    | LetRecMutual of (IdentifierDefn.t * t) list * t
    | Box of Context.t * t (*box (x:A, y:B |- e)*)
    | LetBox of MetaIdentifier.t * t * t (*let box u = e in e'*)
    | Closure of MetaIdentifier.t * t list (*u with (e1, e2, e3, ...)*)
    | Constr of Constructor.t * t option (* Constr e*)
    | Match of t * (Pattern.t * t) list
    | Lift of Typ.t * t
    | EValue of Value.t
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.Expr.t -> t

  val populate_index :
    t ->
    current_ast_level:int ->
    current_identifiers:int String_map.t ->
    current_meta_ast_level:int ->
    current_meta_identifiers:int String_map.t ->
    t Or_error.t

  val shift_indices :
    t ->
    obj_depth:int ->
    meta_depth:int ->
    obj_offset:int ->
    meta_offset:int ->
    t Or_error.t

  val to_val : t -> Value.t option
end

and Value : sig
  type t =
    | Constant of Constant.t (*c*)
    | Prod of t list (*(e, e')*)
    | Left of Typ.t * Typ.t * t (*L[A,B] e*)
    | Right of Typ.t * Typ.t * t (*R[A,B] e*)
    | Lambda of IdentifierDefn.t * Expr.t (*fun (x : A) -> e*)
    | Box of Context.t * Expr.t (*box (x:A, y:B |- e)*)
    | Constr of Constructor.t * t option
  [@@deriving sexp, show, compare, equal]

  val to_expr : Value.t -> Expr.t
end

and Directive : sig
  type t = Reset | Env | Quit [@@deriving sexp, show, compare, equal]

  val of_past : Past.Directive.t -> t
end

and TopLevelDefn : sig
  type t =
    | Definition of IdentifierDefn.t * Expr.t
    | RecursiveDefinition of IdentifierDefn.t * Expr.t
    | MutualRecursiveDefinition of (IdentifierDefn.t * Expr.t) list
    | Expression of Expr.t
    | Directive of Directive.t
    | DatatypeDecl of
        (TypeIdentifier.t * (Constructor.t * Typ.t option) list) list
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.TopLevelDefn.t -> t
end

and Program : sig
  type t = TopLevelDefn.t list [@@deriving sexp, show, compare, equal]

  val of_past : Past.Program.t -> t
end

module TypedTopLevelDefn : sig
  type t =
    | Definition of Typ.t * IdentifierDefn.t * Expr.t
    | RecursiveDefinition of Typ.t * IdentifierDefn.t * Expr.t
    | MutualRecursiveDefinition of (Typ.t * IdentifierDefn.t * Expr.t) list
    | Expression of Typ.t * Expr.t
    | Directive of Directive.t
    | DatatypeDecl of
        (TypeIdentifier.t * (Constructor.t * Typ.t option) list) list
  [@@deriving sexp, show, compare, equal]

  val populate_index : t -> t Or_error.t
  val convert_from_untyped_without_typecheck : TopLevelDefn.t -> t
end

module TypedProgram : sig
  type t = TypedTopLevelDefn.t list [@@deriving sexp, show, compare, equal]

  val populate_index : t -> t Or_error.t
  val convert_from_untyped_without_typecheck : Program.t -> t
end