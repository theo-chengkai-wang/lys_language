open Core
open Lys_parsing
open Lys_typing
open Lys_interpreter
open Lys_ast

let loop interpreter filename () =
  In_channel.with_file filename ~f:(fun file_ic ->
      file_ic |> Lexing.from_channel |> Lex_and_parse.parse_program
      |> Ast.Program.of_past |> Typecore.type_check_program |> ok_exn
      |> Ast.TypedProgram.populate_index |> ok_exn
      |> Interpreter.evaluate_program ~interpreter
      |> ok_exn
      |> fun l ->
      let _ =
        List.iter l ~f:(fun res ->
            print_endline
              (Interpreter_common.TopLevelEvaluationResult.get_str_output res);
            print_endline "")
      in
      ())

let () =
  Command.basic_spec ~summary:"Execute the program."
    Command.Spec.(
      empty
      +> flag "interpreter"
           (required Interpreter.arg_type)
           ~doc:
             "Interpreter choices: choose from m (multi-step), mt (timed \
              multi-step), s (single step), ss (step-counted single_step), ssv \
              (step-counted verbose single step)."
      +> anon ("filename" %: string))
    loop
  |> Command_unix.run
