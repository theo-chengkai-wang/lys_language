open Core
open Core_bench

let () =
  Random.self_init ();
  let x = Random.float 10.0 in
  let y = Random.float 10.0 in
  let tests =
    [
      Bench.Test.create ~name:"Float add" (fun () -> ignore (x +. y));
      Bench.Test.create ~name:"Float mul" (fun () -> ignore (x *. y));
      Bench.Test.create ~name:"Float div" (fun () -> ignore (x /. y));
      Bench.Test.create_indexed ~name:"Array.create"
        ~args:[ 1; 10; 100; 200; 300; 400 ] (fun len ->
          Staged.stage (fun () -> ignore (Array.create ~len 0)));
    ]
  in
  Bench.bench
    ~run_config:
      (Bench.Run_config.create ~quota:(Bench.Quota.Num_calls 500)
         ~stabilize_gc_between_runs:true ())
    ~analysis_configs:(List.map ~f:Bench.Analysis_config.with_error_estimation Bench.Analysis_config.default)
    ~display_config:
      (Bench.Display_config.create ~show_absolute_ci:true
         ())
    tests
