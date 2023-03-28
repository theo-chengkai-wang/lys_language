(*Test bench*)

open Core
open Lys_benchmarks
open Lys_benchmarks.Bench_defns

let (additional_benchmarks2 : base_benchmark_record_legacy list) =
  [
    (*Expected 1h*)
    {
      base_program_loc = "test/example_programs/simple_programs/hello_world.lys";
      run = 1;
      benchmarks =
        List.map
          [ (10, [ 1; 2; 4; 8; 16; 32; 64; 128; 256 ]) ]
          ~f:(fun (exponent, length_list) ->
            List.map length_list ~f:(fun length ->
                let random_list =
                  List.init length ~f:(fun _ -> Random.int Int.max_value)
                in
                let xs =
                  Benchmark_utils.print_int_list random_list "Cons" "Nil"
                in
                [
                  {
                    persist = false;
                    name = Printf.sprintf "map_pow_%i_%i_run" exponent length;
                    body =
                      Printf.sprintf "map_int_int (pow (%i)) (%s);;" exponent xs;
                  };
                  {
                    persist = false;
                    name = Printf.sprintf "map_pow_%i_%i_staged" exponent length;
                    body =
                      Printf.sprintf
                        "let pow_compiled: int -> int = let box u = pow2 (%i) \
                         in fun (x: int) -> u with (x);;\n\
                        \                         map_int_int (pow_compiled) \
                         (%s);;"
                        exponent xs;
                  };
                ])
            |> List.concat)
        |> List.concat;
    };
  ]

let () =
  Command.basic_spec ~summary:"Run Benchmarks and save to file."
    Command.Spec.(empty +> anon ("filename" %: string))
    (fun filename () -> Bench_legacy.bench additional_benchmarks2 filename)
  |> Command_unix.run
