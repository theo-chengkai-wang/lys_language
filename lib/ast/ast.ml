type identifier = string
type meta_identifier = string

type typ =
  | TBool
  | TInt
  | TFun of typ * typ
  | TBox of context * typ
  | TProd of typ * typ
  | TSum of typ * typ

and identifier_defn = identifier * typ (*x : A*)
and context = identifier_defn list (*\Psi*)

type binary_op =
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

type unary_op = NEG | NOT
type constant = Integer of int | Boolean of bool

type expression =
  | Identifier of identifier (*x*)
  | Constant of constant (*c*)
  | UnaryOp of unary_op * expression (*unop e*)
  | BinaryOp of binary_op * expression * expression (*e op e'*)
  | Prod of expression * expression (*(e, e')*)
  | Fst of expression (*fst e*)
  | Snd of expression (*snd e*)
  | Left of typ * typ * expression (*L[A,B] e*)
  | Right of typ * typ * expression (*R[A,B] e*)
  | Match of expression * identifier_defn * expression * identifier_defn * expression
    (*match e with
      L (x: A) -> e' | R (y: B) -> e'' translates to 1 expr and 2 lambdas*)
  | Lambda of identifier_defn * expression (*fun (x : A) -> e*)
  | Application of expression * expression (*e e'*)
  | IfThenElse of expression * expression * expression (*if e then e' else e''*)
  | LetBinding of
      identifier_defn * expression * expression (*let x: A = e in e'*)
  | LetRec of identifier_defn * expression * expression
    (*let rec f: A->B =
      e[f] in e'*)
  | Box of context * expression (*box (x:A, y:B |- e)*)
  | LetBox of meta_identifier * expression * expression (*let box u = e in e'*)
  | Closure of meta_identifier * expression list
(*u with (e1, e2, e3, ...)*)

(*inspired from Cornell course,used as directives to the REPL loop*)
type directive = Reset | Env | Quit

type top_level_defn =
  | Definition of identifier_defn * expression
  | Expression of expression
  | Directive of directive

type program = top_level_defn list
