open Bench_defns
open Core_bench

val default_display_config : Bench.Display_config.t

val compile_bench_legacy :
  ?analysis_configs:Bench.Analysis_config.t list ->
  Legacy.Base_benchmark_record_legacy.t list ->
  (Bench.Run_config.t * Bench.Analysis_config.t list * Bench.Test.t list) list

val compile_bench :
  ?analysis_configs:Bench.Analysis_config.t list ->
  Current.Benchmark_config.base_benchmark_record list ->
  (Bench.Run_config.t * Bench.Analysis_config.t list * Bench.Test.t list) list

val run_bench_exn :
  (Bench.Run_config.t * Bench.Analysis_config.t list * Bench.Test.t list) list ->
  Bench.Analysis_result.t list

val translate_bench_results_exn :
  Bench.Analysis_result.t list -> Current.Benchmark_result.t list

val run_and_translate :
  (Core_bench_internals.Run_config.t
  * Bench.Analysis_config.t list
  * Core_bench_internals.Test.t list)
  list -> Current.Benchmark_result.t list

val bench_display_exn_legacy :
  ?display_config:Bench.Display_config.t ->
  Legacy.Base_benchmark_record_legacy.t list ->
  unit

val bench_exn_legacy :
  Legacy.Base_benchmark_record_legacy.t list -> Current.Benchmark_result.t list

val bench_display_exn :
  ?display_config:Bench.Display_config.t ->
  Current.Benchmark_config.base_benchmark_record list ->
  unit

val bench_exn :
  Current.Benchmark_config.base_benchmark_record list -> Current.Benchmark_result.t list
