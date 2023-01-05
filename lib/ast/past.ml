open Core

module type Identifier_type = sig
  (*Uniform identifier type for all identifiers*)
  type t = string [@@deriving sexp, show, equal, compare]
end

module Constructor : sig
  type t = string [@@deriving sexp, show, equal, compare]
end = struct
  type t = string [@@deriving sexp, show, equal, compare]
end

module rec Identifier : Identifier_type = struct
  type t = string [@@deriving sexp, show, equal, compare]
end

and Typ : sig
  type t =
    | TUnit
    | TBool
    | TInt
    | TIdentifier of Identifier.t
    | TFun of t * t
    | TBox of Context.t * t
    | TProd of t list
    | TSum of t * t
  [@@deriving sexp, show, equal, compare]
end = struct
  type t =
    | TUnit
    | TBool
    | TInt
    | TIdentifier of Identifier.t
    | TFun of t * t
    | TBox of Context.t * t
    | TProd of t list
    | TSum of t * t
  [@@deriving sexp, show, equal, compare]
end

and IdentifierDefn : sig
  type t = Identifier.t * Typ.t [@@deriving sexp, show, equal, compare]
end = struct
  type t = Identifier.t * Typ.t [@@deriving sexp, show, equal, compare]
end

and Context : sig
  type t = IdentifierDefn.t list [@@deriving sexp, show, equal, compare]
end = struct
  type t = IdentifierDefn.t list [@@deriving sexp, show, equal, compare]
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
  [@@deriving sexp, show, equal, compare]
end = struct
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
  [@@deriving sexp, show, equal, compare]
end

and UnaryOperator : sig
  type t = NEG | NOT [@@deriving sexp, show, equal, compare]
end = struct
  type t = NEG | NOT [@@deriving sexp, show, equal, compare]
end

and Constant : sig
  type t = Integer of int | Boolean of bool | Unit
  [@@deriving sexp, show, equal, compare]
end = struct
  type t = Integer of int | Boolean of bool | Unit
  [@@deriving sexp, show, equal, compare]
end

and Pattern : sig
  type t =
    | Datatype of (Constructor.t * Identifier.t list)
      (*Empty list means that data type doesn't have arguments.*)
    | Inl of Identifier.t
    | Inr of Identifier.t
    | Prod of Identifier.t list
    | Id of Identifier.t
    | Wildcard
  [@@deriving sexp, show, equal, compare]
end = struct
  type t =
    | Datatype of (Constructor.t * Identifier.t list)
    | Inl of Identifier.t
    | Inr of Identifier.t
    | Prod of Identifier.t list
    | Id of Identifier.t
    | Wildcard
  [@@deriving sexp, show, equal, compare]
  (*Prior to support for polymorphism we have a separate Inl and Inr thing
    TODO: Make support multi-level patterns
    TODO: Support wildcards properly
  *)
end

and Expr : sig
  type t =
    | Identifier of Identifier.t (*x*)
    | Constant of Constant.t (*c*)
    | UnaryOp of UnaryOperator.t * t (*unop e*)
    | BinaryOp of BinaryOperator.t * t * t (*e op e'*)
    | Prod of t list (*(e, e')*)
    (*| Fst of t (*fst e*)
    | Snd of t (*snd e*)*)
    | Nth of (t * int)
    | Left of Typ.t * Typ.t * t (*L[A,B] e*)
    | Right of Typ.t * Typ.t * t (*R[A,B] e*)
    | Case of t * IdentifierDefn.t * t * IdentifierDefn.t * t
      (*case e with
        L (x: A) -> e' | R (y: B) -> e'' translates to 1 expr and 2 lambdas*)
    | Lambda of IdentifierDefn.t * t (*fun (x : A) -> e*)
    | Application of t * t (*e e'*)
    | IfThenElse of t * t * t (*if e then e' else e''*)
    | LetBinding of IdentifierDefn.t * t * t (*let x: A = e in e'*)
    | LetRec of IdentifierDefn.t * t * t
      (*let rec f: A->B =
        e[f] in e'*)
    | Box of Context.t * t (*box (x:A, y:B |- e)*)
    | LetBox of Identifier.t * t * t (*let box u = e in e'*)
    | Closure of Identifier.t * t list (*u with (e1, e2, e3, ...)*)
    | Constr of Constructor.t * t option (* Constr e*)
    | Match of t * (Pattern.t * t) list
  [@@deriving sexp, show, equal, compare]
end = struct
  type t =
    | Identifier of Identifier.t (*x*)
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
      (*case e with
        L (x: A) -> e' | R (y: B) -> e'' translates to 1 expr and 2 lambdas*)
    | Lambda of IdentifierDefn.t * t (*fun (x : A) -> e*)
    | Application of t * t (*e e'*)
    | IfThenElse of t * t * t (*if e then e' else e''*)
    | LetBinding of IdentifierDefn.t * t * t (*let x: A = e in e'*)
    | LetRec of IdentifierDefn.t * t * t
      (*let rec f: A->B =
        e[f] in e'*)
    | Box of Context.t * t (*box (x:A, y:B |- e)*)
    | LetBox of Identifier.t * t * t (*let box u = e in e'*)
    | Closure of Identifier.t * t list (*u with (e1, e2, e3, ...)*)
    | Constr of Constructor.t * t option (* Constr e*)
    | Match of t * (Pattern.t * t) list
      (*match e with pattern -> ... | ... -> ... | ... -> ... | ...*)
  [@@deriving sexp, show, equal, compare]
end

and Directive : sig
  type t = Reset | Env | Quit [@@deriving sexp, show, equal, compare]
end = struct
  type t = Reset | Env | Quit [@@deriving sexp, show, equal, compare]
end

and TopLevelDefn : sig
  type t =
    | Definition of IdentifierDefn.t * Expr.t
    | RecursiveDefinition of IdentifierDefn.t * Expr.t
    | Expression of Expr.t
    | Directive of Directive.t
    | DatatypeDecl of Identifier.t * (Constructor.t * (Typ.t option)) list
  [@@deriving sexp, show, equal, compare]
end = struct
  type t =
    | Definition of IdentifierDefn.t * Expr.t
    | RecursiveDefinition of IdentifierDefn.t * Expr.t
    | Expression of Expr.t
    | Directive of Directive.t
    | DatatypeDecl of Identifier.t * (Constructor.t * (Typ.t option)) list
  [@@deriving sexp, show, equal, compare]
end

and Program : sig
  type t = TopLevelDefn.t list [@@deriving sexp, show, equal, compare]
end = struct
  type t = TopLevelDefn.t list [@@deriving sexp, show, equal, compare]
end
