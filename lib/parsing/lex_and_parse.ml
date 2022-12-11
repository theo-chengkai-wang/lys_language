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
      fprintf stderr "%a: Parsing error at character \"%s\" \n" print_position
        lexbuf (Lexing.lexeme lexbuf);
      exit (-1)

let parse_program = parse_with_error

let parse_expression lexbuf =
  (*Utility function: only parse the first element if it is an expression*)
  let tree = parse_program lexbuf in
  match tree with
  | Lys_ast.Past.TopLevelDefn.Expression expr :: _ -> Some expr
  | _ -> None

let parse_and_print lexbuf =
  let tree_list = parse_program lexbuf in
  List.fold tree_list ~init:() ~f:(fun _ top_level ->
      print_endline (Lys_ast.Past.TopLevelDefn.show top_level);
      print_endline "")
