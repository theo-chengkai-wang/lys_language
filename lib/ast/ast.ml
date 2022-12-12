open Core

module type Identifier_type = sig
  (*Hide implementation*)
  type t [@@deriving sexp, show, compare, equal]

  val of_string : string -> t
  val of_past : Past.Identifier.t -> t
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

module rec ObjIdentifier : ObjIdentifier_type = struct
  type t = string [@@deriving sexp, show, compare, equal]

  let of_string x = x
  let of_past (past_identifier : Past.Identifier.t) = past_identifier
end

and MetaIdentifier : MetaIdentifier_type = struct
  type t = string [@@deriving sexp, show, compare, equal]

  let of_string x = x
  let of_past (past_identifier : Past.Identifier.t) = past_identifier
end

and TypeIdentifier : TypeIdentifier_type = struct
  type t = string [@@deriving sexp, show, compare, equal]

  let of_string x = x
  let of_past (past_identifier : Past.Identifier.t) = past_identifier
end

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

  val of_past : Past.Typ.t -> t
end = struct
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

  let rec of_past = function
    | Past.Typ.TUnit -> TUnit
    | Past.Typ.TBool -> TBool
    | Past.Typ.TInt -> TInt
    | Past.Typ.TIdentifier id -> TIdentifier (TypeIdentifier.of_past id)
    | Past.Typ.TFun (t1, t2) -> TFun (of_past t1, of_past t2)
    | Past.Typ.TBox (ctx, t1) -> TBox (Context.of_past ctx, of_past t1)
    | Past.Typ.TProd (t1, t2) -> TProd (of_past t1, of_past t2)
    | Past.Typ.TSum (t1, t2) -> TSum (of_past t1, of_past t2)
end

and IdentifierDefn : sig
  type t = ObjIdentifier.t * Typ.t [@@deriving sexp, show, compare, equal]

  val of_past : Past.IdentifierDefn.t -> t
end = struct
  type t = ObjIdentifier.t * Typ.t [@@deriving sexp, show, compare, equal]

  let of_past (id, t1) = (ObjIdentifier.of_past id, Typ.of_past t1)
end

and Context : sig
  type t = IdentifierDefn.t list [@@deriving sexp, show, compare, equal]

  val of_past : Past.Context.t -> t
end = struct
  type t = IdentifierDefn.t list [@@deriving sexp, show, compare, equal]

  let of_past ident_defn_list =
    List.map ident_defn_list ~f:IdentifierDefn.of_past
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

  val of_past : Past.BinaryOperator.t -> t
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
  [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.BinaryOperator.ADD -> ADD
    | Past.BinaryOperator.SUB -> SUB
    | Past.BinaryOperator.MUL -> MUL
    | Past.BinaryOperator.DIV -> DIV
    | Past.BinaryOperator.MOD -> MOD
    | Past.BinaryOperator.EQ -> EQ
    | Past.BinaryOperator.NEQ -> NEQ
    | Past.BinaryOperator.GTE -> GTE
    | Past.BinaryOperator.GT -> GT
    | Past.BinaryOperator.LTE -> LTE
    | Past.BinaryOperator.LT -> LT
    | Past.BinaryOperator.AND -> AND
    | Past.BinaryOperator.OR -> OR
end

and UnaryOperator : sig
  type t = NEG | NOT [@@deriving sexp, show, compare, equal]

  val of_past : Past.UnaryOperator.t -> t
end = struct
  type t = NEG | NOT [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.UnaryOperator.NEG -> NEG
    | Past.UnaryOperator.NOT -> NOT
end

and Constant : sig
  type t = Integer of int | Boolean of bool | Unit
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.Constant.t -> t
end = struct
  type t = Integer of int | Boolean of bool | Unit
  [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.Constant.Integer i -> Integer i
    | Past.Constant.Boolean b -> Boolean b
    | Past.Constant.Unit -> Unit
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

  val of_past : Past.Expr.t -> t
end = struct
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

  let rec of_past = function
    | Past.Expr.Identifier id -> Identifier (ObjIdentifier.of_past id)
    | Past.Expr.Constant c -> Constant (Constant.of_past c)
    | Past.Expr.UnaryOp (op, expr) ->
        UnaryOp (UnaryOperator.of_past op, of_past expr)
    | Past.Expr.BinaryOp (op, expr, expr2) ->
        BinaryOp (BinaryOperator.of_past op, of_past expr, of_past expr2)
    | Past.Expr.Prod (expr1, expr2) -> Prod (of_past expr1, of_past expr2)
    | Past.Expr.Fst expr -> Fst (of_past expr)
    | Past.Expr.Snd expr -> Snd (of_past expr)
    | Past.Expr.Left (t1, t2, expr) ->
        Left (Typ.of_past t1, Typ.of_past t2, of_past expr)
    | Past.Expr.Right (t1, t2, expr) ->
        Right (Typ.of_past t1, Typ.of_past t2, of_past expr)
    | Past.Expr.Match (e, iddef1, e1, iddef2, e2) ->
        Match
          ( of_past e,
            IdentifierDefn.of_past iddef1,
            of_past e1,
            IdentifierDefn.of_past iddef2,
            of_past e2 )
    | Past.Expr.Lambda (iddef, e) ->
        Lambda (IdentifierDefn.of_past iddef, of_past e)
    | Past.Expr.Application (e1, e2) -> Application (of_past e1, of_past e2)
    | Past.Expr.IfThenElse (b, e1, e2) ->
        IfThenElse (of_past b, of_past e1, of_past e2)
    | Past.Expr.LetBinding (iddef, e, e2) ->
        LetBinding (IdentifierDefn.of_past iddef, of_past e, of_past e2)
    | Past.Expr.LetRec (iddef, e, e2) ->
        LetRec (IdentifierDefn.of_past iddef, of_past e, of_past e2)
    | Past.Expr.Box (ctx, e) -> Box (Context.of_past ctx, of_past e)
    | Past.Expr.LetBox (metaid, e, e2) ->
        LetBox (MetaIdentifier.of_past metaid, of_past e, of_past e2)
    | Past.Expr.Closure (metaid, exprs) ->
        Closure (MetaIdentifier.of_past metaid, List.map exprs ~f:of_past)
end

and Directive : sig
  type t = Reset | Env | Quit [@@deriving sexp, show, compare, equal]

  val of_past : Past.Directive.t -> t
end = struct
  type t = Reset | Env | Quit [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.Directive.Reset -> Reset
    | Past.Directive.Env -> Env
    | Past.Directive.Quit -> Quit
end

and TopLevelDefn : sig
  type t =
    | Definition of IdentifierDefn.t * Expr.t
    | RecursiveDefinition of IdentifierDefn.t * Expr.t
    | Expression of Expr.t
    | Directive of Directive.t
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.TopLevelDefn.t -> t
end = struct
  (*Note added type for defns (not useful for now but useful for when adding inference) and exprs for the REPL*)
  type t =
    | Definition of IdentifierDefn.t * Expr.t
    | RecursiveDefinition of IdentifierDefn.t * Expr.t
    | Expression of Expr.t
    | Directive of Directive.t
  [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.TopLevelDefn.Definition (iddef, e) ->
        Definition (IdentifierDefn.of_past iddef, Expr.of_past e)
    | Past.TopLevelDefn.RecursiveDefinition (iddef, e) ->
        RecursiveDefinition (IdentifierDefn.of_past iddef, Expr.of_past e)
    | Past.TopLevelDefn.Expression e -> Expression (Expr.of_past e)
    | Past.TopLevelDefn.Directive d -> Directive (Directive.of_past d)
end

and Program : sig
  type t = TopLevelDefn.t list [@@deriving sexp, show, compare, equal]

  val of_past : Past.Program.t -> t
end = struct
  type t = TopLevelDefn.t list [@@deriving sexp, show, compare, equal]

  let of_past progs = List.map progs ~f:TopLevelDefn.of_past
end

module TypedTopLevelDefn : sig
  type t =
    | Definition of Typ.t * IdentifierDefn.t * Expr.t
    | RecursiveDefinition of Typ.t * IdentifierDefn.t * Expr.t
    | Expression of Typ.t * Expr.t
    | Directive of Directive.t
  [@@deriving sexp, show, compare, equal]
end = struct
  (*Note added type for defns (not useful for now but useful for when adding inference) and exprs for the REPL*)
  type t =
    | Definition of Typ.t * IdentifierDefn.t * Expr.t
    | RecursiveDefinition of Typ.t * IdentifierDefn.t * Expr.t
    | Expression of Typ.t * Expr.t
    | Directive of Directive.t
  [@@deriving sexp, show, compare, equal]
end

module TypedProgram : sig
  type t = TypedTopLevelDefn.t list [@@deriving sexp, show, compare, equal]
end = struct
  type t = TypedTopLevelDefn.t list [@@deriving sexp, show, compare, equal]
end
