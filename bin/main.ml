(* let () = print_endline "Hello, World!" *)
(* My test bench *)

open Core
open Lys_parsing
open Lys_typing
open Lys_utils
open Lys_ast
open Lys_interpreter

let loop filename () =
  In_channel.with_file filename ~f:(fun file_ic ->
      file_ic |> Lexing.from_channel |> Lex_and_parse.parse_program
      |> Typecore.type_check_program |> ok_exn 
      |> Interpreter.evaluate_program |> ok_exn
      |> fun l ->
      let _ =
        List.map l ~f:(fun res ->
            print_endline
              (Interpreter.TopLevelEvaluationResult.get_str_output res);
            print_endline "")
      in
      ())

let () =
  Command.basic_spec ~summary:"Type check the input program and display it."
    Command.Spec.(empty +> anon ("filename" %: string))
    loop
  |> Command_unix.run
