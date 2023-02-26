open Bench_defns
open Core_bench

val compile_bench_legacy :
  ?analysis_configs:Bench.Analysis_config.t list ->
  base_benchmark_record_legacy list ->
  (Bench.Run_config.t * Bench.Analysis_config.t list * Bench.Test.t list) list

val run_bench_exn :
  (Bench.Run_config.t * Bench.Analysis_config.t list * Bench.Test.t list) list ->
  Bench.Analysis_result.t list

val translate_bench_results_exn :
  Bench.Analysis_result.t list -> benchmark_result_record list

val bench_display_exn_legacy :
  ?display_config:Bench.Display_config.t -> base_benchmark_record_legacy list -> unit

val bench_to_csv_exn_legacy : base_benchmark_record_legacy list -> Csv.t
