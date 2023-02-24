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
