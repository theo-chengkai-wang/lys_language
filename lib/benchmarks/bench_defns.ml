open Core

type benchmark_program = { name : string; body : string; persist : bool }
[@@deriving show, sexp]

type base_benchmark_record = {
  base_program_loc : string;
  run : int;
  benchmarks : benchmark_program list;
}
[@@deriving show, sexp]

type benchmark_result_record_legacy = {
  base_program_loc : string;
  run_id : int;
  benchmark : benchmark_program;
  defn_id : int;
  steps : int;
  time : float;
}
[@@deriving show, sexp]

type data_with_interval = {
  r_2 : float;
  mean : float;
  abs_lo_diff : float;
  abs_hi_diff : float;
}
[@@deriving show, sexp]

type benchmark_result_record = {
  bench_name: string;
  time_per_run : data_with_interval;
  mWd_per_run : data_with_interval;
  mjWd_per_run : data_with_interval;
  promotions_per_run : data_with_interval;
  mGC_per_run : data_with_interval;
  mjGC_per_run : data_with_interval;
  compactions_per_run : data_with_interval;
}
[@@deriving show, sexp]
