module type Identifier_type = sig
  type t [@@deriving sexp, show, compare, equal]

  val of_string : string -> t
end

module type ObjIdentifier_type = sig
  include Identifier_type
end

module type TypeIdentifier_type = sig
  (*Unused for now*)
  include Identifier_type
end

module type MetaIdentifier_type = sig
  include Identifier_type
end

module rec ObjIdentifier : ObjIdentifier_type
and MetaIdentifier : MetaIdentifier_type
and TypeIdentifier : TypeIdentifier_type

and Typ : sig
  type t =
    | TUnit
    | TBool
    | TInt
    | TIdentifier of TypeIdentifier.t
    | TFun of t * t
    | TBox of Context.t * t
    | TProd of t * t
    | TSum of t * t
  [@@deriving sexp, show, compare, equal]
end

and IdentifierDefn : sig
  type t = ObjIdentifier.t * Typ.t [@@deriving sexp, show, compare, equal]
end

and Context : sig
  type t = IdentifierDefn.t list [@@deriving sexp, show, compare, equal]
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
  [@@deriving sexp, show, compare, equal]
end

and UnaryOperator : sig
  type t = NEG | NOT [@@deriving sexp, show, compare, equal]
end

and Constant : sig
  type t = Integer of int | Boolean of bool | Unit [@@deriving sexp, show, compare, equal]
end

and Expr : sig
  type t =
    | Identifier of ObjIdentifier.t (*x*)
    | Constant of Constant.t (*c*)
    | UnaryOp of UnaryOperator.t * t (*unop e*)
    | BinaryOp of BinaryOperator.t * t * t (*e op e'*)
    | Prod of t * t (*(e, e')*)
    | Fst of t (*fst e*)
    | Snd of t (*snd e*)
    | Left of Typ.t * Typ.t * t (*L[A,B] e*)
    | Right of Typ.t * Typ.t * t (*R[A,B] e*)
    | Match of t * IdentifierDefn.t * t * IdentifierDefn.t * t
      (*match e with
        L (x: A) -> e' | R (y: B) -> e'' translates to 1 expr and 2 lambdas*)
    | Lambda of IdentifierDefn.t * t (*fun (x : A) -> e*)
    | Application of t * t (*e e'*)
    | IfThenElse of t * t * t (*if e then e' else e''*)
    | LetBinding of IdentifierDefn.t * t * t (*let x: A = e in e'*)
    | LetRec of IdentifierDefn.t * t * t
      (*let rec f: A->B =
        e[f] in e'*)
    | Box of Context.t * t (*box (x:A, y:B |- e)*)
    | LetBox of MetaIdentifier.t * t * t (*let box u = e in e'*)
    | Closure of MetaIdentifier.t * t list (*u with (e1, e2, e3, ...)*)
  [@@deriving sexp, show, compare, equal]
end

and Directive : sig
  type t = Reset | Env | Quit [@@deriving sexp, show, compare, equal]
end

and TopLevelDefn : sig
  type t =
    | Definition of Typ.t * IdentifierDefn.t * Expr.t
    | RecursiveDefinition of Typ.t * IdentifierDefn.t * Expr.t
    | Expression of Typ.t * Expr.t
    | Directive of Directive.t
  [@@deriving sexp, show, compare, equal]
end

and Program : sig
  type t = TopLevelDefn.t list [@@deriving sexp, show, compare, equal]
end
