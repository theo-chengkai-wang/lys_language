(* let () = print_endline "Hello, World!" *)
(* My test bench *)

open Core
open Lys_parsing
open Lys_typing
open Lys_ast

let loop str () =
  str |>
    Lexing.from_string
    |> Lex_and_parse.parse_program
    |> Typecore.type_check_program 
    |> ok_exn
    |> Ast.TypedProgram.show
    |> print_endline

let () = loop "" ()

