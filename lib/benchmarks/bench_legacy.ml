open Core
open Lys_interpreter
open Bench_defns
open Benchmark_utils

(*Step * Time list for each expression executed, and in a list.*)

let rec perform_run steps_only run_cnt base_program_loc { name; body; persist }
    eval_ctx typ_ctx run_id =
  print_endline
    ("... ... benchmarking: " ^ name ^ "; run: " ^ Int.to_string run_id);
  (*Performs run_cnt number of runs and get both the step cnt and the time measurement*)
  if run_id >= run_cnt then
    (*Compute new context if we have persist = true*)
    if persist then
      let _, new_eval_ctx, new_typ_context =
        evaluate_from_lexbuf_given_context
          (Interpreter.MultiStep { show_time = false })
          (Lexing.from_string body) eval_ctx typ_ctx
      in
      ([], new_eval_ctx, new_typ_context)
    else ([], eval_ctx, typ_ctx)
  else
    (*Step*)
    let top_level_results, _, _ =
      evaluate_from_lexbuf_given_context
        (Interpreter.SingleStep { show_step_count = true; verbose = false })
        (Lexing.from_string body) eval_ctx typ_ctx
    in
    let results_step =
      List.map top_level_results ~f:(fun top_level ->
          get_top_level_non_rec_step_count_exn top_level)
    in
    let results_time =
      (*If only steps we only treat steps*)
      if steps_only then List.map results_step ~f:(fun _ -> 0.)
      else
        (*Time*)
        let top_level_results, _, _ =
          evaluate_from_lexbuf_given_context
            (Interpreter.MultiStep { show_time = true })
            (Lexing.from_string body) eval_ctx typ_ctx
        in
        List.map top_level_results ~f:(fun top_level ->
            get_top_level_non_rec_time_exn top_level)
    in
    List.zip_exn results_step results_time
    |> List.mapi ~f:(fun i (steps, time) ->
           {
             base_program_loc;
             run_id;
             benchmark = { name; body; persist };
             defn_id = i;
             steps;
             time;
           })
    |> fun res ->
    let rest, new_eval_ctx, new_typ_context =
      perform_run steps_only run_cnt base_program_loc { name; body; persist }
        eval_ctx typ_ctx (run_id + 1)
    in
    (res @ rest, new_eval_ctx, new_typ_context)

let do_benchmark ?(steps_only = false)
    ({ base_program_loc; run; benchmarks } : base_benchmark_record) =
  (*do a benchmark*)
  print_endline
    (Printf.sprintf "---------------------- \n Fetching: %s" base_program_loc);
  let _, eval_ctx, typ_ctx =
    In_channel.with_file base_program_loc ~f:(fun file_ic ->
        evaluate_from_lexbuf_given_context
          (Interpreter.MultiStep { show_time = false })
          (Lexing.from_channel file_ic)
          Interpreter_common.EvaluationContext.empty
          Interpreter_common.TypeConstrContext.empty)
  in
  List.fold benchmarks ~init:([], eval_ctx, typ_ctx)
    ~f:(fun (current_list, eval_ctx, typ_ctx) bm ->
      print_endline ("... benchmarking " ^ bm.name);
      perform_run steps_only run base_program_loc bm eval_ctx typ_ctx 0
      |> fun (results, new_eval_ctx, new_typ_ctx) ->
      (current_list @ results, new_eval_ctx, new_typ_ctx))
  |> fun (results, _, _) -> results

let perform_all_benchmarks ?(steps_only = false) bms =
  List.concat_map bms ~f:(do_benchmark ~steps_only)

let bench ?(steps_only = false) benchmarks filename =
  perform_all_benchmarks ~steps_only benchmarks |> to_csv |> Csv.save filename
