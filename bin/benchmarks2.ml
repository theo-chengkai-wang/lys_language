open Core
open Core_bench

let bench_without_display ~run_config ~analysis_configs tests = 
  tests |> Bench.measure ~run_config 
  |> List.map ~f:(Bench.analyze ~analysis_configs)
  |> Or_error.combine_errors
  |> ok_exn

let () =
  Random.self_init ();
  let x = Random.float 10.0 in
  let y = Random.float 10.0 in
  let tests =
    [
      Bench.Test.create ~name:"Float add" (fun () -> ignore (x +. y));
      (* Bench.Test.create ~name:"Float mul" (fun () -> ignore (x *. y));
      Bench.Test.create ~name:"Float div" (fun () -> ignore (x /. y));
      Bench.Test.create_indexed ~name:"Array.create"
        ~args:[ 1; 10; 100; 200; 300; 400 ] (fun len ->
          Staged.stage (fun () -> ignore (Array.create ~len 0))); *)
    ]
  in
  Bench.bench
    ~run_config:
      (Bench.Run_config.create ~quota:(Bench.Quota.Num_calls 10000)
         ~stabilize_gc_between_runs:true ())
    ~analysis_configs:
      (List.map ~f:Bench.Analysis_config.with_error_estimation
         Bench.Analysis_config.default)
  ~display_config:(Bench.Display_config.create ~show_all_values:true ~ascii_table:true ~show_absolute_ci:true ())
    tests
  (* |> [%sexp_of: Bench.Analysis_result.t list]
  |> Sexp.to_string_hum
  |> print_endline *)
