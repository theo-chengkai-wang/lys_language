open Core
open Lexing

(* let print_position outx lexbuf =
   let pos = lexbuf.lex_curr_p in
   fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
     (pos.pos_cnum - pos.pos_bol + 1)
*)
let get_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* let parse_with_error lexbuf =
   try Parser.start Lexer.read lexbuf with
   | SyntaxError msg ->
       fprintf stderr "%a: Syntax error: %s\n" print_position lexbuf msg;
       exit (-1)
   | Parser.Error ->
       fprintf stderr "%a: Parsing error at character \"%s\" \n" print_position
         lexbuf (Lexing.lexeme lexbuf);
       exit (-1) *)

(* Version with exceptions given *)
exception SyntaxError of string
exception ParsingError of string

let parse_program lexbuf =
  try Parser.start Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
      raise
        (SyntaxError
           (Printf.sprintf "%s: Syntax error: %s" (get_position lexbuf) msg))
  | Parser.Error ->
      raise
        (ParsingError
           (Printf.sprintf "%s: Parsing error at character: %s"
              (get_position lexbuf) (Lexing.lexeme lexbuf)))

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
