(*Benchmarks*)

open Core
open Lys_benchmarks
open Lys_benchmarks.Bench_defns

let benchmarks =
  [
    {
      base_program_loc = "test/example_programs/simple_programs/hello_world.lys";
      run = 10000;
      name = "pow";
      program =
        {
          program_run = "pow";
          program_compile = "pow2";
          program_staged_name = "p";
          compiled_type = "[x:int]int";
        };
      arguments =
        [
          (2, [ 1; 2; 5; 10; 20; 50; 100 ]);
          (5, [ 1; 2; 5; 10; 20; 50; 100 ]);
          (10, [ 1; 2; 5; 10; 20; 50; 100 ]);
          (50, [ 1; 2; 5; 10; 20; 50; 100 ]);
        ]
        |> List.map ~f:(fun (stage_0, stage_1_list) ->
               let str_stage_0 = Int.to_string stage_0 in
               let str_stage_1 =
                 List.map
                   ~f:(fun i ->
                     { name = Some (Int.to_string i); body = Int.to_string i })
                   stage_1_list
               in
               ({ name = Some str_stage_0; body = str_stage_0 }, str_stage_1));
    };
    {
      base_program_loc = "test/example_programs/simple_programs/convolution.lys";
      run = 1000;
      program =
        {
          program_run = "conv_";
          program_compile = "conv_staged_";
          program_staged_name = "p";
          compiled_type = "[ys: intlist]intlist";
        };
      name = "conv";
      arguments =
        [ 5; 10; 20; 50; 100; 200 ]
        |> List.map ~f:(fun i ->
               ( {
                   name = Some (Int.to_string i);
                   body =
                     Benchmark_utils.print_int_list
                       (Benchmark_utils.random_list i)
                       "Cons" "Nil";
                 },
                 [
                   {
                     name = Some (Int.to_string i);
                     body =
                       Benchmark_utils.print_int_list
                         (Benchmark_utils.random_list i)
                         "Cons" "Nil";
                   };
                 ] ));
    };
    {
      name = "regexp";
      base_program_loc = "test/example_programs/regexp/regexp.lys";
      run = 1;
      program =
        {
          program_run = "accept1";
          program_compile = "accept2";
          program_staged_name = "p";
          compiled_type = "[str: string]bool";
        };
      arguments =
        (let regexps =
           [
             "Times (\n\
             \            Plus (\n\
             \                Times (\n\
             \                    Const ('1'), \n\
             \                    Times (Const ('2'), Empty)\n\
             \                ), \n\
             \                Empty\n\
             \            ), \n\
             \            Const ('3')\n\
             \          )";
             "Times (Plus (Const ('1'), Const ('2')), Times (Const ('3'), \
              Const ('4')))";
             "Times (Const ('0'), Star (Times (Star (Const ('a')), \
              Const('b'))))";
             "Star (Star (Const ('1')))";
           ]
         in
         let strs = [ "111"; "0abaab"; "0abab0"; "0abba"; "0"; "" ] in
         let mapped_strs =
           List.mapi
             ~f:(fun i s ->
               {
                 name = Some (Int.to_string i);
                 body = Printf.sprintf "\"%s\"" s;
               })
             strs
         in
         List.mapi
           ~f:(fun i regexp ->
             ({ name = Some (Int.to_string i); body = regexp }, mapped_strs))
           regexps);
    };
    {
      base_program_loc = "test/example_programs/while_language/while.lys";
      run = 100;
      name = "while";
      program =
        {
          program_run = "interpret";
          program_compile = "interpret_staged";
          program_staged_name = "compiled";
          compiled_type = "[args: int_list]int_list";
        };
      arguments =
        [
          ( { name = Some "fib_program"; body = "fib_program" },
            [ 1; 2; 5; 10; 20; 50; 100 ]
            |> List.map ~f:(fun i ->
                   {
                     name = Some (Int.to_string i);
                     body =
                       Benchmark_utils.print_int_list [ i ] "Cons_i" "Nil_i";
                   }) );
        ];
    };
    {
      base_program_loc = "test/example_programs/flowchart/flowchart.lys";
      run = 100;
      name = "flowchart";
      program =
        {
          program_run = "interpret";
          compiled_type = "[args: int_list]int";
          program_compile = "interpret_staged";
          program_staged_name = "compiled";
        };
      arguments =
        [
          ( { name = Some "fib_program"; body = "fib_program" },
            [ 1; 2; 5; 10; 20; 50; 100 ]
            |> List.map ~f:(fun i ->
                   {
                     name = Some (Int.to_string i);
                     body =
                       Benchmark_utils.print_int_list [ i ] "Cons_i" "Nil_i";
                   }) );
        ];
    };
  ]

let () =
  Command.basic_spec ~summary:"Run Benchmarks and save to file."
    Command.Spec.(
      empty
      +> flag "save" (optional string) ~aliases:[ "s" ] ~doc:"Save to file")
    (fun filename_opt () ->
      match filename_opt with
      | None -> Bench_cb.bench_display_exn benchmarks
      | Some filename ->
          Bench_cb.bench_to_csv_exn benchmarks |> Csv.save filename)
  |> Command_unix.run
