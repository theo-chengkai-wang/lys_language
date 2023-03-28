open Core

type benchmark_program_legacy = { name : string; body : string; persist : bool }
[@@deriving show, sexp]

type base_benchmark_record_legacy = {
  base_program_loc : string;
  run : int;
  benchmarks : benchmark_program_legacy list;
}
[@@deriving show, sexp]

type benchmark_result_record_legacy = {
  base_program_loc : string;
  run_id : int;
  benchmark : benchmark_program_legacy;
  defn_id : int;
  steps : int;
  time : float;
}
[@@deriving show, sexp]

type benchmark_program = {
  program_run : string; (*argument: usually bench_unstaged %s %s*)
  program_compile : string;
  (*Type -> Term -> String program to compile, usually let x:typ = term;;*)
  compiled_type : string;
  program_staged_name : string;
      (*usually let box u = staged compile in u with (blah)*)
}

type argument = { body : string; name : string option }

type base_benchmark_record = {
  name : string;
  base_program_loc : string;
  run : int;
  program : benchmark_program;
  arguments : (argument * argument list) list;
}

type data_with_interval = {
  r_2 : float;
  mean : float;
  abs_lo_diff : float;
  abs_hi_diff : float;
}
[@@deriving show, sexp]

type benchmark_result_record = {
  bench_name : string;
  time_per_run : data_with_interval;
  mWd_per_run : data_with_interval;
  mjWd_per_run : data_with_interval;
  promotions_per_run : data_with_interval;
  mGC_per_run : data_with_interval;
  mjGC_per_run : data_with_interval;
  compactions_per_run : data_with_interval;
}
[@@deriving show, sexp]
