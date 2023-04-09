%{
open Lys_ast.Past
(*open Core*)

(*TODO: ATTEMPT Constructs an Application AST node from an expression with the function and a list of expressions f is applied to
exception EmptyListError

let mkapp f xs = 
    let rev_xs = List.rev xs in 
    let rec mkapp_aux f rxs = 
        match rxs with
        | [x] -> Application (f, x)
        | x::rxs -> Application (mkapp_aux f rxs, x)
        | [] -> raise EmptyListError
    in 
    mkapp_aux f rev_xs*)

(*let list_to_tuple_aux xs init = List.fold xs ~init:init ~f:(fun acc -> fun x -> Expr.Prod (acc, x))

let list_to_tuple l = 
    match l with
    | [] -> Expr.Constant (Constant.Unit)
    | [x] -> x
    | x::xs -> list_to_tuple_aux xs x*)
%}

%token <int> INT
%token <string> ID
%token <string> CONSTR
%token <char> CHAR
%token <string> STRING
%token <string> TYPEVAR
%token BOOL_typ
%token INT_typ
%token STRING_typ
%token ARR_typ
%token AND_WORD "and"
%token CHAR_typ
%token UNIT_typ
%token FORALL
%token EXISTS
%token QUOTE
%token ARR_OPEN
%token ARR_CLOSE
%token EOL ";;"
%token REF
%token DEREF "!"
%token ASSIGN ":="
%token DOT "."
%token ARR_LEN
%token LEFT_ARROW "<-"
%token UNIT
%token TRUE
%token FALSE 
%token NOT
%token AND
%token OR
%token STRING_CONCAT "^"
%token CHAR_STRING_CONCAT "++"
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token MOD "%"
%token UNDERSCORE
%token LIFT
%token GT ">"
%token GTE ">="
%token LT "<"
%token LTE "<="
%token EQ "="
%token NEQ "!="
%token IF
%token THEN
%token ELSE
%token FUN
%token ARROW "->"
%token COMMA ","
%token SEMICOLON ";"
%token LEFT_PAREN "("
%token RIGHT_PAREN ")"
// %token FST
// %token SND
%token INL
%token INR
%token LEFT_BRACKET "["
%token RIGHT_BRACKET "]"
%token MATCH
%token WITH
%token CASE
%token OF
%token LET
%token DATATYPE
%token IN
%token REC
%token TURNSTILE "|-"
%token BOX
%token COLON ":"
// %token SKIPLINE "//" // TODO: DEAL WITH COMMENTS?
%token PATTERN_OR "|"
%token WHILE
%token DO
%token DONE
// %token EOL
%token EOF
%token DIR_RESET
%token DIR_ENV
%token DIR_QUIT

(* Operator Definitions*)
// TODO: refine precedence according to https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly -- ACTUALLY PROPERLY DEAL WITH THIS SHIT PLEASE
// typ sum and prod
%left typ_SUM typ_PRODUCT

// Functions
%right "->"
%right DOT_BIG_LAMBDA

// Bracket
%nonassoc LEFT_BRACKET RIGHT_BRACKET

// Let/in
%nonassoc LET
%right IN

// typ arrow
// %nonassoc typ_FUNCTION_ARROW
// // Match with
// metavariable with
%left WITH

%left PATTERN_OR


%left DEFN_EQ // precedence for = signs in definitions

// Imperative constructs
%nonassoc WHILE
%nonassoc DO
%nonassoc DONE
%right SEMICOLON // for sequencing
%right ASSIGN
%right LEFT_ARROW

// bool
%left AND OR
%nonassoc NOT
// int to bool
%nonassoc ">=" "<=" ">" "<" "=" "!="
// int
%left "+" "-" "^"
%left "*" "/" "%"
%right  "++"

%nonassoc UMINUS
// References = high priority, with Dereferencing = highest priority.
%nonassoc REF
%nonassoc DEREF
%left DOT
%nonassoc ARR_LEN
// Want simple expressions to be at the highest priority
%nonassoc SIMPLE_EXPR
%nonassoc TRUE FALSE INT ID LEFT_PAREN ARR_OPEN

// %start <lys_ast.Ast.program> prog
// TODO temporarily just do expressions
// TODO Specify types for non terminals
%start <Lys_ast.Past.Program.t> start

%%

// rev_prog:
//     | EOF {[]}
//     | p = prog; defn = top_level  { defn::p }

// prog: p = rev_prog {List.rev p}

// top_level
// TODO: to be changed to actual

start: p = prog {p}

prog: 
    | EOF {[]}
    | t = top_level ";;" p = prog {t::p}

datatype_decl_clause: c = CONSTR OF t = typ  {(c, Some t)} | c = CONSTR {(c, None)}

single_rec: decl = id_typ_declaration EQ e1 = expr {(decl, e1)}

mutual_rec:
    | LET REC r = single_rec "and" l=separated_nonempty_list("and", single_rec) {r::l}

datatype_decl:
    | i = identifier EQ l = separated_nonempty_list ("|", datatype_decl_clause) {(i, l)}
    | i = identifier {(i,[])}

top_level:
    | d = directive {TopLevelDefn.Directive d}
    | LET decl = id_typ_declaration EQ e1 = expr {TopLevelDefn.Definition (decl, e1)}
    | LET REC r = single_rec {let (decl, e1) = r in TopLevelDefn.RecursiveDefinition (decl, e1)}
    | m = mutual_rec {TopLevelDefn.MutualRecursiveDefinition (m)}
    | e = expr {TopLevelDefn.Expression e}
    | DATATYPE decl_list = separated_nonempty_list("and", datatype_decl) {TopLevelDefn.DatatypeDecl (decl_list)}

directive:
    | DIR_ENV {Directive.Env}
    | DIR_QUIT {Directive.Quit}
    | DIR_RESET {Directive.Reset}

simple_expr:
    | LEFT_PAREN; e = expr; RIGHT_PAREN {e}
    | c = constant {Expr.Constant c}
    | i = identifier {Expr.Identifier i}
    | LEFT_PAREN e1=expr COMMA l = separated_nonempty_list(COMMA, expr) RIGHT_PAREN {Expr.Prod (e1::l)} (*Hack to ensure at least 2 things*)

application_expr:
    | s = application_expr; "[" t = typ "]" {Expr.TypeApply (s, t)}
    | s = simple_expr; "[" t = typ "]" {Expr.TypeApply (s, t)}
    | s1 = application_expr; s2 = simple_expr  {Expr.Application (s1, s2)}
    | s1 = simple_expr; s2 = simple_expr {Expr.Application (s1, s2)}

pattern: 
    | UNDERSCORE {Pattern.Wildcard}
    | i = identifier {Pattern.Id i}
    | c = CONSTR LEFT_PAREN l = separated_nonempty_list(COMMA, identifier) RIGHT_PAREN {Pattern.Datatype (c, l)}
    | c = CONSTR {Pattern.Datatype (c, [])}
    | INL i=identifier {Pattern.Inl (i)}
    | INR i = identifier {Pattern.Inr (i)}
    | LEFT_PAREN i = identifier COMMA is = separated_nonempty_list(COMMA, identifier) RIGHT_PAREN {Pattern.Prod (i::is)} 
    | c=identifier "++" s=identifier {Pattern.ConcatCharString (c, s)}
    | s = STRING {Pattern.String (s)}
    // SCRAPED Decision is only to support 2-ary products   
    // | LEFT_PAREN i = identifier COMMA i2 = identifier RIGHT_PAREN {Pattern.Prod ([i; i2])}    

;

pattern_expr: p = pattern "->" e = expr {(p, e)};


expr:
    | s = simple_expr {s}
    | a = application_expr {a}
    | r = ref {r}
    | a = array {a}
    | i = imperative {i}
    | a = arith { a }
    | c = comp { c }
    | s = string_op { s }
    | b = bool { b }
    | c = CONSTR e = option(simple_expr) {Expr.Constr (c, e)}
    | LIFT LEFT_BRACKET t = typ RIGHT_BRACKET e = simple_expr {Expr.Lift (t, e)}
    | u = identifier; WITH; s = sim_sub {Expr.Closure (u, s)} // not a simple_expr because it contains a WITH application
    (* bigger constructs *)
    | IF e1 = expr THEN e2 = expr ELSE e3=expr {Expr.IfThenElse (e1, e2, e3)}
    | FUN arg = id_typ_declaration "->" e = expr {Expr.Lambda (arg, e)}
    | v = TYPEVAR DOT e = expr %prec DOT_BIG_LAMBDA {Expr.BigLambda (v, e)}
    | e = simple_expr; LEFT_BRACKET i = INT RIGHT_BRACKET {Expr.Nth(e, i)}
    // | FST e = expr {Expr.Fst e}
    // | SND e = expr {Expr.Snd e}
    | INL LEFT_BRACKET t1=typ COMMA t2=typ RIGHT_BRACKET e = expr {Expr.Left (t1, t2, e)}
    | INR LEFT_BRACKET t1=typ COMMA t2=typ RIGHT_BRACKET e = expr {Expr.Right (t1, t2, e)}
    | CASE e1 = simple_expr OF INL id_decl1 = id_typ_declaration "->" e2 = expr "|" INR id_decl2 = id_typ_declaration "->" e3 = expr {Expr.Case (e1, id_decl1, e2, id_decl2, e3)}
    | LET decl = id_typ_declaration EQ e1 = expr IN e2 = expr %prec DEFN_EQ {Expr.LetBinding (decl, e1, e2)}
    | LET REC r = single_rec IN e2 = expr %prec DEFN_EQ {let (decl, e1) = r in Expr.LetRec (decl, e1, e2)}
    | m = mutual_rec IN e2 = expr %prec DEFN_EQ {Expr.LetRecMutual (m, e2)}
    | BOX LEFT_PAREN decl_list = separated_list(COMMA, id_typ_declaration) TURNSTILE e = expr RIGHT_PAREN {Expr.Box (decl_list, e)}
    | LET BOX u = identifier EQ e1 = expr IN e2 = expr {Expr.LetBox (u, e1, e2)}
    | MATCH e = simple_expr WITH option("|") pattern_list = separated_nonempty_list("|", pattern_expr) {Expr.Match (e, pattern_list)};

constant:
    | i = INT {Constant.Integer i}
    | TRUE {Constant.Boolean true}
    | FALSE {Constant.Boolean false}
    | LEFT_PAREN RIGHT_PAREN {Constant.Unit}
    | c = CHAR {Constant.Character c}
    | s = STRING {Constant.String s}

identifier:
    | i = ID {i};

id_typ_declaration:
    | i = identifier COLON t = typ {(i, t)}
    | LEFT_PAREN d = id_typ_declaration RIGHT_PAREN {d};

sim_sub: 
    | LEFT_PAREN l = separated_list(COMMA, expr) RIGHT_PAREN {l} 

typ:
    | BOOL_typ {Typ.TBool}
    | INT_typ {Typ.TInt}
    | UNIT_typ {Typ.TUnit}
    | CHAR_typ {Typ.TChar}
    | t = typ ARR_typ {Typ.TArray (t)}
    | t = typ REF {Typ.TRef(t)}
    | FORALL v = TYPEVAR DOT t=typ {Typ.TForall (v, t)} // Polymorphic stuff
    | v = TYPEVAR {Typ.TVar (v)}
    | STRING_typ {Typ.TString}
    | i = ID {Typ.TIdentifier (i)}
    | t1 = typ; "->"; t2 = typ /*%prec typ_FUNCTION_ARROW*/ {Typ.TFun (t1, t2)}
    | LEFT_PAREN t = typ "*" ts = separated_nonempty_list("*", typ) RIGHT_PAREN %prec typ_PRODUCT {Typ.TProd (t::ts)}
    | t1 = typ; "+"; t2 = typ %prec typ_SUM {Typ.TSum (t1, t2)}
    | LEFT_BRACKET decl_list = separated_list(COMMA, id_typ_declaration) RIGHT_BRACKET t = typ {Typ.TBox (decl_list, t)}
    | LEFT_PAREN t = typ RIGHT_PAREN {t};

arith:
    | e1 = expr "+" e2 = expr {Expr.BinaryOp (BinaryOperator.ADD, e1, e2)}
    | e1 = expr "-" e2 = expr {Expr.BinaryOp (BinaryOperator.SUB, e1, e2)}
    | e1 = expr "*" e2 = expr {Expr.BinaryOp (BinaryOperator.MUL, e1, e2)}
    | e1 = expr "/" e2 = expr {Expr.BinaryOp (BinaryOperator.DIV, e1, e2)}
    | e1 = expr "%" e2 = expr {Expr.BinaryOp (BinaryOperator.MOD, e1, e2)}
    | "-" e = expr %prec UMINUS {Expr.UnaryOp (UnaryOperator.NEG, e)};

comp:
    | e1 = expr ">" e2 = expr {Expr.BinaryOp (GT, e1, e2)}
    | e1 = expr "<" e2 = expr {Expr.BinaryOp (LT, e1, e2)}
    | e1 = expr ">=" e2 = expr {Expr.BinaryOp (GTE, e1, e2)}
    | e1 = expr "<=" e2 = expr {Expr.BinaryOp (LTE, e1, e2)}
    | e1 = expr "=" e2 = expr {Expr.BinaryOp (EQ, e1, e2)}
    | e1 = expr "!=" e2 = expr {Expr.BinaryOp (NEQ, e1, e2)};

bool:
    | e1 = expr AND e2 = expr {Expr.BinaryOp (AND, e1, e2)}
    | e1 = expr OR e2 = expr {Expr.BinaryOp (OR, e1, e2)}
    | NOT e = expr {Expr.UnaryOp (NOT, e)};

ref: 
    | e1 = expr ASSIGN e2 = expr {Expr.BinaryOp(ASSIGN, e1, e2)}
    | "!"e = expr {Expr.UnaryOp(DEREF, e)}
    | REF e = expr {Expr.Ref(e)};

imperative:
    | e1 = expr ";" e2 = expr {Expr.BinaryOp(SEQ, e1, e2)}
    | WHILE p = expr DO e2 = expr DONE {Expr.While(p, e2)};

array:
    | ARR_OPEN l = separated_list (",", expr) ARR_CLOSE {Expr.Array (l)}
    | e1 = expr ".""(" e2 = expr ")" {Expr.BinaryOp (ARRAY_INDEX, e1, e2)}
    | ARR_LEN e = simple_expr {Expr.UnaryOp (ARRAY_LEN, e)}
    | e1 = expr ".""(" e2 = expr ")" "<-" e3 = expr {Expr.ArrayAssign (e1, e2, e3)}

string_op:
    | e1 = expr "^" e2 = expr {Expr.BinaryOp (STRINGCONCAT, e1, e2)}
    | e1 = expr "++" e2 = expr {Expr.BinaryOp (CHARSTRINGCONCAT, e1, e2)}
