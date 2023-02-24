open Core
open Benchmark_utils
open Bench_defns
open Lys_interpreter
open Core_bench

let compile_single_benchmark_program { name; body; persist } eval_ctx typ_ctx =
  print_endline ("... ... compiling benchmark: " ^ name);
  let test =
    Bench.Test.create ~name (fun () ->
        evaluate_from_lexbuf_given_context
          (Interpreter.MultiStep { show_time = true })
          (Lexing.from_string body) eval_ctx typ_ctx)
  in
  if persist then
    let _, new_eval_ctx, new_typ_context =
      evaluate_from_lexbuf_given_context
        (Interpreter.MultiStep { show_time = false })
        (Lexing.from_string body) eval_ctx typ_ctx
    in
    (test, new_eval_ctx, new_typ_context)
  else (test, eval_ctx, typ_ctx)

let compile_base_record ~analysis_configs { base_program_loc; run; benchmarks }
    =
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
      print_endline ("... compiling benchmark " ^ bm.name);
      compile_single_benchmark_program bm eval_ctx typ_ctx
      |> fun (results, new_eval_ctx, new_typ_ctx) ->
      (results :: current_list, new_eval_ctx, new_typ_ctx))
  |> fun (results, _, _) ->
  ( Bench.Run_config.create ~quota:(Bench.Quota.Num_calls run) (),
    List.map ~f:Bench.Analysis_config.with_error_estimation analysis_configs,
    results )

let compile_bench ?(analysis_configs = Bench.Analysis_config.default) =
  List.map ~f:(compile_base_record ~analysis_configs)

let bench_without_display_or_error ~run_config ~analysis_configs tests =
  tests |> Bench.measure ~run_config
  |> List.map ~f:(Bench.analyze ~analysis_configs)
  |> Or_error.combine_errors

let run_bench_exn
    (compiled_tests :
      (Bench.Run_config.t * Bench.Analysis_config.t list * Bench.Test.t list)
      list) =
  List.concat_map compiled_tests ~f:(fun (rc, acs, tests) ->
      bench_without_display_or_error ~run_config:rc ~analysis_configs:acs tests
      |> ok_exn)

let default_display_config =
  Bench.Display_config.create ~ascii_table:true ~show_absolute_ci:true ()

let bench_display_exn ?(display_config = default_display_config) bench_list =
  bench_list |> compile_bench |> run_bench_exn |> Bench.display ~display_config
