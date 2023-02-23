open Core
open Lys_parsing
open Lys_typing
open Lys_interpreter
open Lys_ast
open Bench_defns

let evaluate_from_lexbuf_given_context interpreter lexbuf context typ_context =
  lexbuf |> Lex_and_parse.parse_program |> Ast.Program.of_past
  |> Typecore.type_check_program
       ~obj_ctx:
         (Interpreter_common.EvaluationContext.to_typing_obj_context context)
       ~type_ctx:
         (Interpreter_common.TypeConstrContext.to_typeconstrtypingcontext
            typ_context)
  |> ok_exn |> Ast.TypedProgram.populate_index |> ok_exn
  |> Interpreter.evaluate_top_level_defns ~top_level_context:context
       ~type_constr_context:typ_context ~interpreter
  |> ok_exn

(*Helper for convolution*)
let rec random_list len =
  if len = 0 then [] else Random.int 2048 :: random_list (len - 1)

(*Helper for turning int lists into*)
let rec print_int_list l cons nil =
  match l with
  | [] -> nil
  | x :: xs -> Printf.sprintf "%s (%i, %s)" cons x (print_int_list xs cons nil)

(*
  Accumulate a list of program paths zipped with a list of expr * int
  Execute that expr int times to get a list of such values
  (THINK CSV AGAIN PLEASE) -- maybe have record entry.
*)

(*Print to CSV utils*)

let to_csv results =
  List.map results
    ~f:(fun { base_program_loc; run_id; benchmark; defn_id; steps; time } ->
      [
        base_program_loc;
        Int.to_string run_id;
        benchmark.name;
        Int.to_string defn_id;
        Int.to_string steps;
        Float.to_string time;
      ])
  |> fun l ->
  (*Header*)
  [ "base_program"; "run_id"; "benchmark"; "defn_id"; "steps"; "time" ] :: l

let get_top_level_non_rec_step_count_exn t =
  match t with
  | Interpreter_common.TopLevelEvaluationResult.Defn (_, _, _, Some v, _) -> v
  | Interpreter_common.TopLevelEvaluationResult.ExprValue (_, _, _, Some v, _)
    ->
      v
  | _ -> failwith "Not a valid top level evaluation result"

let get_top_level_non_rec_time_exn t =
  match t with
  | Interpreter_common.TopLevelEvaluationResult.Defn (_, _, Some v, _, _) -> v
  | Interpreter_common.TopLevelEvaluationResult.ExprValue (_, _, Some v, _, _)
    ->
      v
  | _ -> failwith "Not a valid top level evaluation result"
