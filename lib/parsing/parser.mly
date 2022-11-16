%{
    open Lys_ast.Ast
%}

%token <int> INT
%token <string> ID
%token <string> META_ID // not sure
%token BOOL_typ
%token INT_typ
%token TRUE
%token FALSE 
%token NOT
%token AND
%token OR
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token MOD "%"
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
%token LEFT_PAREN "("
%token RIGHT_PAREN ")"
%token FST
%token SND
%token INL
%token INR
%token LEFT_BRACKET "["
%token RIGHT_BRACKET "]"
%token MATCH
%token WITH
%token LET
%token IN
%token REC
%token TURNSTILE "|-"
%token BOX
%token COLON ":"
%token SKIPLINE "//"
%token PATTERN_OR "|"
%token EOL
%token EOF

(* Operator Definitions*)
// TODO: refine precedence according to https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly
// typs
%right typ_FUNCTION_ARROW
%left typ_SUM typ_PRODUCT
// Functions
%right "->"
%left APP

%left PATTERN_OR
%left DEFN_EQ // precedence for = signs in definitions
// bool
%left AND OR
%nonassoc NOT
// int to bool
%nonassoc ">=" "<=" ">" "<" "=" "!="
// int
%left "+" "-"
%left "*" "/"
%nonassoc UMINUS

// %start <lys_ast.Ast.program> prog
// TODO temporarily just do expressions
// TODO Specify types for non terminals
%start <Lys_ast.Ast.expression option> start

%%

// rev_prog:
//     | EOF {[]}
//     | p = prog; defn = top_level  { defn::p }

// prog: p = rev_prog {List.rev p}

// top_level

// TODO: How to make application WITH SPACES left-associative?
// https://github.com/Timothy-G-Griffin/cc_cl_cam_ac_uk/blob/master/slang/parser.mly
// Solution? https://stackoverflow.com/questions/2847399/on-ocamlyacc-function-application-grammar-and-precedence

start:
    | EOF {None}
    | e = expr EOL EOF {Some e}

expr:
    | LEFT_PAREN; e = expr; RIGHT_PAREN {e}
    | c = constant {Constant c}
    | i = identifier {Identifier i}
    | a = arith { a }
    | b = bool { b }
    | c = comp { c }
    (* bigger constructs *)
    | IF e1 = expr THEN e2 = expr ELSE e3=expr {IfThenElse (e1, e2, e3)}
    | FUN arg = id_typ_declaration "->" e = expr {Lambda (arg, e)}
    | e1 = expr e2 = expr %prec APP {Application (e1, e2)}
    | LEFT_PAREN e1 = expr COMMA e2 = expr RIGHT_BRACKET {Prod (e1, e2)}
    | FST e = expr {Fst e}
    | SND e = expr {Snd e}
    | INL LEFT_BRACKET t1=typ COMMA t2=typ RIGHT_BRACKET e = expr {Left (t1, t2, e)}
    | INR LEFT_BRACKET t1=typ COMMA t2=typ RIGHT_BRACKET e = expr {Right (t1, t2, e)}
    | MATCH e1 = expr WITH INL id_decl1 = id_typ_declaration "->" e2 = expr "|" INR id_decl2 = id_typ_declaration "->" e3 = expr {Match (e1, id_decl1, e2, id_decl2, e3)}
    | LET decl = id_typ_declaration EQ e1 = expr IN e2 = expr %prec DEFN_EQ {LetBinding (decl, e1, e2)}
    | LET REC decl = id_typ_declaration EQ e1 = expr IN e2 = expr %prec DEFN_EQ {LetRec (decl, e1, e2)}
    | BOX LEFT_PAREN decl_list = separated_list(COMMA, id_typ_declaration) TURNSTILE e = expr RIGHT_PAREN {Box (decl_list, e)}
    | LET BOX u = meta_identifier EQ e1 = expr IN e2 = expr {LetBox (u, e1, e2)}
    | u = meta_identifier WITH s = sim_sub {Closure (u, s)}

constant:
    | i = INT {Integer i}
    | TRUE {Boolean true}
    | FALSE {Boolean false};

identifier:
    | i = ID {i};

meta_identifier:
    | u = META_ID {u};

id_typ_declaration:
    | i = identifier COLON t = typ {(i, t)};

sim_sub: LEFT_PAREN l = separated_list(COMMA, expr) RIGHT_PAREN {l};

typ:
    | BOOL_typ {TBool}
    | INT_typ {TInt}
    | t1 = typ; "->"; t2 = typ %prec typ_FUNCTION_ARROW {TFun (t1, t2)}
    | t1 = typ; "*"; t2 = typ %prec typ_PRODUCT {TFun (t1, t2)}
    | t1 = typ; "+"; t2 = typ %prec typ_SUM {TFun (t1, t2)};

arith:
    | e1 = expr "+" e2 = expr {BinaryOp (ADD, e1, e2)}
    | e1 = expr "-" e2 = expr {BinaryOp (SUB, e1, e2)}
    | e1 = expr "*" e2 = expr {BinaryOp (MUL, e1, e2)}
    | e1 = expr "/" e2 = expr {BinaryOp (DIV, e1, e2)}
    | e1 = expr "%" e2 = expr {BinaryOp (MOD, e1, e2)}
    | "-" e = expr %prec UMINUS {UnaryOp (NEG, e)};

comp:
    | e1 = expr ">" e2 = expr {BinaryOp (GT, e1, e2)}
    | e1 = expr "<" e2 = expr {BinaryOp (LT, e1, e2)}
    | e1 = expr ">=" e2 = expr {BinaryOp (GTE, e1, e2)}
    | e1 = expr "<=" e2 = expr {BinaryOp (LTE, e1, e2)}
    | e1 = expr "=" e2 = expr {BinaryOp (EQ, e1, e2)}
    | e1 = expr "!=" e2 = expr {BinaryOp (NEQ, e1, e2)};

bool:
    | e1 = expr AND e2 = expr {BinaryOp (AND, e1, e2)}
    | e1 = expr OR e2 = expr {BinaryOp (OR, e1, e2)}
    | NOT e = expr {UnaryOp (NOT, e)};
