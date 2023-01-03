open Core
open Lys_parsing
open Lys_typing
open Lys_ast

let loop filename () =
  In_channel.with_file filename ~f:(fun file_ic ->
    file_ic |>
    Lexing.from_channel
    |> Lex_and_parse.parse_program
    |> Ast.Program.of_past
    |> Typecore.type_check_program 
    |> ok_exn
    |> Ast.TypedProgram.show
    |> print_endline
      )

let () =
  Command.basic_spec ~summary:"Type check the input program and display it."
    Command.Spec.(empty +> anon ("filename" %: string))
    loop
  |> Command_unix.run
