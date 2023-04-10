module Legacy : sig
  module Benchmark_program_legacy: sig
    type t = { name : string; body : string; persist : bool }
    [@@deriving show, sexp, fields, csv]
  end

  module Base_benchmark_record_legacy : sig
    type t = {
      base_program_loc : string;
      run : int;
      benchmarks : Benchmark_program_legacy.t list;
    }
    [@@deriving show, sexp]
  end

  module Benchmark_result_record_legacy : sig
    type t = {
      base_program_loc : string;
      run_id : int;
      benchmark : Benchmark_program_legacy.t;
      defn_id : int;
      steps : int;
      time : float;
    }
    [@@deriving show, sexp, fields, csv]
  end
end

module Current : sig
  module Benchmark_config : sig
    type benchmark_program = {
      program_run : string; (*argument: usually bench_unstaged %s %s*)
      program_compile : string;
      (*Type -> Term -> String program to compile, usually let x:typ = term;;*)
      compiled_type : string;
      program_staged_name : string;
    }

    type argument = { body : string; name : string option }

    type base_benchmark_record = {
      name : string;
      base_program_loc : string;
      run : int;
      program : benchmark_program;
      arguments : (argument * argument list) list;
    }
  end

  module Benchmark_result : sig
    type data_with_interval = {
      r_2 : float;
      mean : float;
      abs_lo_diff : float;
      abs_hi_diff : float;
    }
    [@@deriving show, sexp, fields, csv]
  
    type t = {
      bench_name : string;
      time_per_run : data_with_interval;
      mWd_per_run : data_with_interval;
      mjWd_per_run : data_with_interval;
      promotions_per_run : data_with_interval;
      mGC_per_run : data_with_interval;
      mjGC_per_run : data_with_interval;
      compactions_per_run : data_with_interval;
    }
    [@@deriving show, sexp, fields, csv]
  
  end

end
