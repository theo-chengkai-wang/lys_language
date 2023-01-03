open Core
open Lys_parsing
open Lys_typing
open Lys_ast
open Lys_interpreter

let loop program context () =
  program |> Lexing.from_string |> Lex_and_parse.parse_program
  |> Ast.Program.of_past
  |> Typecore.type_check_program ~obj_ctx:(Interpreter.EvaluationContext.to_typing_obj_context context) |> ok_exn
  |> Interpreter.evaluate_top_level_defns ~top_level_context:context
  |> ok_exn

let read_line () =
  Out_channel.(flush stdout);
  In_channel.(input_line_exn stdin)

let main_loop () =
  let continue = ref true in
  let accumulated_program = ref "" in
  let evaluation_context = ref Interpreter.EvaluationContext.empty in
  print_string "# ";
  while !continue do
    let line = read_line () in
    accumulated_program := !accumulated_program ^ line;
    if String.is_suffix line ~suffix:";;" then (
      (*execute*)
      let res =
        Or_error.try_with (loop !accumulated_program !evaluation_context)
      in
      print_endline "-------------------";
      let () =
        match res with
        | Error e ->
            print_endline "ERROR";
            print_endline (Error.to_string_hum e)
        | Ok (res, new_context) ->
            (List.iter res ~f:(fun res ->
                 print_endline
                   (Interpreter.TopLevelEvaluationResult.get_str_output res);
                 print_endline "");
             (*Now just stop when we have to stop*)
             let last_res_opt = List.last res in
             match last_res_opt with
             | None -> ()
             | Some last_res -> (
                 match last_res with
                 | Interpreter.TopLevelEvaluationResult.Directive
                     (Ast.Directive.Quit, _) ->
                     continue := false
                 | _ -> ()));
            evaluation_context := new_context
      in
      accumulated_program := "";
      print_endline "-------------------";
      print_string "# ")
    else ()
  done

let () = main_loop ()
