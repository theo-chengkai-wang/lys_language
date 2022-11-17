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
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      None
  | Parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let parse_expression = parse_with_error

let parse_and_print lexbuf =
  let parsed_tree = parse_expression lexbuf in
  match parsed_tree with
  | None -> print_endline "Parsing failed."
  | Some tree -> print_endline (Lys_ast.Ast.show_expression tree)
