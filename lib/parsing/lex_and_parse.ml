open Core
open Lexing
open Lexer

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.start Lexer.read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: Syntax error: %s\n" print_position lexbuf msg;
      exit (-1)
  | Parser.Error ->
      fprintf stderr ("%a: Parsing error at character \"%s\" \n") print_position lexbuf (Lexing.lexeme lexbuf);
      exit (-1)

let parse_program = parse_with_error

let parse_expression lexbuf = 
  (*Parse the first expression*)
  let tree = parse_program lexbuf in
  match tree with
  |(Expression expr)::_ -> Some expr
  | _ -> None

let parse_and_print lexbuf =
  (*TODO: Modify this to actual impl for the entire program*)
  let parsed_tree = parse_program lexbuf in
  match parsed_tree with
  | (Expression expr)::_ -> print_endline (Lys_ast.Ast.show_expression expr)
  | _ -> print_endline "End of parse"
