{
(* HEAVY INSPIRATION from Ch. 16 of "Real Workd Ocaml" *) 
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

(*From OCaml impl https://stackoverflow.com/questions/66307896/lexing-strings-in-ocamllex*)
let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

}

let nat = ['0'-'9'] ['0'-'9']*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* (*Lower case is normal id*)
let constr_id = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* (*Upper case is Constr id*)

let backslash_escapes =
    ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule read = 
    parse
    | white {read lexbuf}
    | newline  { next_line lexbuf; read lexbuf }
    | "(*" {comment lexbuf}
    | "'" {character lexbuf}
    | ";" {SEMICOLON}
    | "true" {TRUE}
    | "false" {FALSE}
    | "bool" {BOOL_typ}
    | "int" {INT_typ}
    | "char" {CHAR_typ}
    | "unit" {UNIT_typ}
    | "!" {NOT}
    | "&&" {AND}
    | "||" {OR}
    | "lift" {LIFT}
    | "+" {PLUS}
    | "-" {MINUS}
    | "*" {TIMES}
    | "/" {DIV}
    | "%" {MOD}
    | ">" {GT}
    | ">=" {GTE}
    | "<" {LT}
    | "<=" {LTE}
    | "=" {EQ}
    | "!=" {NEQ}
    | "->" {ARROW}
    | "," {COMMA}
    | "(" {LEFT_PAREN}
    | ")" {RIGHT_PAREN}
    | "[" {LEFT_BRACKET}
    | "]" {RIGHT_BRACKET}
    | "|-" {TURNSTILE}
    | ":" {COLON}
    | "//" {next_line lexbuf; read lexbuf}
    | "|" {PATTERN_OR}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "fun" {FUN}
    (* | "fst" {FST}
    | "snd" {SND} *)
    | "L" {INL}
    | "R" {INR}
    | "match" {MATCH}
    |  "with" {WITH}
    | "case" {CASE}
    | "of" {OF}
    | "let" {LET}
    | "in" {IN}
    | "rec" {REC}
    | "box" {BOX}
    | "RESET" {DIR_RESET}
    | "ENV" {DIR_ENV}
    | "QUIT" {DIR_QUIT}
    | "datatype" {DATATYPE}
    | "and" {AND_WORD}
    | "_" {UNDERSCORE}
    | nat   { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | id    {ID (Lexing.lexeme lexbuf)}
    | constr_id {CONSTR (Lexing.lexeme lexbuf)}
    | eof {EOF}
    | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf))}
and comment = 
  parse
  | "*)" {read lexbuf}
  | newline  { next_line lexbuf; comment lexbuf }
  | _ (*skip*) {comment lexbuf}
and character = 
  parse
  | '\\' (backslash_escapes as c) '\'' {CHAR (char_for_backslash c)}
  | _ as c '\'' {CHAR (char_for_backslash c)}
