(*Benchmarks*)

open Core
open Lys_benchmarks
open Lys_benchmarks.Bench_defns

let benchmarks_legacy =
  [
    {
      base_program_loc = "test/example_programs/simple_programs/hello_world.lys";
      run = 10000;
      benchmarks =
        [
          (2, 3);
          (2, 4);
          (2, 5);
          (2, 10);
          (2, 20);
          (5, 3);
          (5, 8);
          (5, 16);
          (10, 2);
          (10, 3);
          (10, 10);
          (50, 2);
          (50, 10);
        ]
        |> List.concat_map ~f:(fun (n, x) ->
               [
                 {
                   name = Printf.sprintf "pow_%i_%i" n x;
                   body = Printf.sprintf "pow %i %i;;" n x;
                   persist = false;
                 };
                 {
                   name = Printf.sprintf "pow_%i_%i_compile" n x;
                   body = Printf.sprintf "let p: [x:int]int = pow2 %i;;" n;
                   persist = true;
                 };
                 {
                   name = Printf.sprintf "pow_%i_%i_staged" n x;
                   body = Printf.sprintf "let box u = p in u with (%i);;" x;
                   persist = false;
                 };
               ]);
    };
    {
      base_program_loc = "test/example_programs/simple_programs/convolution.lys";
      run = 1000;
      benchmarks =
        [
          (Benchmark_utils.random_list 5, Benchmark_utils.random_list 5);
          (Benchmark_utils.random_list 10, Benchmark_utils.random_list 10);
          (Benchmark_utils.random_list 100, Benchmark_utils.random_list 100);
          (Benchmark_utils.random_list 200, Benchmark_utils.random_list 200);
        ]
        |> List.concat_map ~f:(fun (xs, ys) ->
               let i = List.length xs in
               let xs_str = Benchmark_utils.print_int_list xs "Cons" "Nil" in
               let ys_str = Benchmark_utils.print_int_list ys "Cons" "Nil" in
               [
                 {
                   name = Printf.sprintf "conv_%i" i;
                   body =
                     Printf.sprintf
                       "conv (%s) (%s) (fun (ys: intlist) -> Nil);;" xs_str
                       ys_str;
                   persist = false;
                 };
                 {
                   name = Printf.sprintf "conv_%i_compile" i;
                   body =
                     Printf.sprintf
                       "let p: [ys: intlist]intlist = conv_staged (%s) (box \
                        (ys: intlist|- Nil));;"
                       xs_str;
                   persist = true;
                 };
                 {
                   name = Printf.sprintf "conv_%i_staged" i;
                   body = Printf.sprintf "let box u = p in u with (%s);;" ys_str;
                   persist = false;
                 };
               ]);
    };
    {
      base_program_loc = "test/example_programs/regexp/regexp.lys";
      run = 500;
      benchmarks =
        (let regexps =
           [
             ( "Times (\n\
               \            Plus (\n\
               \                Times (\n\
               \                    Const ('1'), \n\
               \                    Times (Const ('2'), Empty)\n\
               \                ), \n\
               \                Empty\n\
               \            ), \n\
               \            Const ('3')\n\
               \          )",
               1 );
             ( "Times (Plus (Const ('1'), Const ('2')), Times (Const ('3'), \
                Const ('4')))",
               2 );
             ( "Times (Const ('0'), Star (Times (Star (Const ('a')), \
                Const('b'))))",
               3 );
             ("Star (Star (Const ('1')))", 4);
           ]
         in
         let strs =
           [
             ("111", 1);
             ("0abaab", 2);
             ("0abab0", 3);
             ("0abba", 4);
             ("0", 5);
             ("", 6);
           ]
         in
         List.cartesian_product regexps strs
         |> List.concat_map ~f:(fun ((regexp, i), (str, j)) ->
                [
                  {
                    name = Printf.sprintf "regexp_%i_%i" i j;
                    body = Printf.sprintf "accept1 (%s) (\"%s\");;" regexp str;
                    persist = false;
                  };
                  {
                    name = Printf.sprintf "regexp_%i_%i_compile" i j;
                    body =
                      Printf.sprintf "let p: [str: string]bool = accept2 (%s);;"
                        regexp;
                    persist = true;
                  };
                  {
                    name = Printf.sprintf "regexp_%i_%i_staged" i j;
                    body =
                      Printf.sprintf "let box u = p in u with (\"%s\");;" str;
                    persist = false;
                  };
                ]));
    };
    {
      base_program_loc = "test/example_programs/while_language/while.lys";
      run = 100;
      benchmarks =
        (let programs_and_inputs =
           [
             ( "fib_program",
               [ 1; 2; 5; 10; 20; 50; 100 ]
               |> List.map ~f:(fun x_l ->
                      Benchmark_utils.print_int_list [ x_l ] "Cons_i" "Nil_i")
             );
           ]
         in
         List.concat_map programs_and_inputs
           ~f:(fun (program_name, program_args) ->
             List.concat_map program_args ~f:(fun arg ->
                 [
                   {
                     name =
                       Printf.sprintf "while_interpret_%s_%s" program_name arg;
                     body =
                       Printf.sprintf "interpret (%s) (%s);;" program_name arg;
                     persist = false;
                   };
                   {
                     name =
                       Printf.sprintf "while_interpret_compile_%s" program_name;
                     body =
                       Printf.sprintf
                         "let compiled: [args: int_list]int_list = \
                          interpret_staged %s;;"
                         program_name;
                     persist = true;
                   };
                   {
                     name =
                       Printf.sprintf "while_interpret_staged_%s_%s"
                         program_name arg;
                     body =
                       Printf.sprintf "let box u = compiled in u with (%s);;"
                         arg;
                     persist = false;
                   };
                 ])));
    };
    {
      base_program_loc = "test/example_programs/flowchart/flowchart.lys";
      run = 100;
      benchmarks =
        (let programs_and_inputs =
           [
             ( "fib_program",
               [ 1; 2; 5; 10; 20; 50; 100 ]
               |> List.map ~f:(fun x_l ->
                      Benchmark_utils.print_int_list [ x_l ] "Cons_i" "Nil_i")
             );
           ]
         in
         List.concat_map programs_and_inputs
           ~f:(fun (program_name, program_args) ->
             List.concat_map program_args ~f:(fun arg ->
                 [
                   {
                     name =
                       Printf.sprintf "flowchart_interpret_%s_%s" program_name
                         arg;
                     body =
                       Printf.sprintf "interpret (%s) (%s);;" program_name arg;
                     persist = false;
                   };
                   {
                     name =
                       Printf.sprintf "flowchart_interpret_compile_%s"
                         program_name;
                     body =
                       Printf.sprintf
                         "let compiled: [args: int_list]int = interpret_staged \
                          %s;;"
                         program_name;
                     persist = true;
                   };
                   {
                     name =
                       Printf.sprintf "flowchart_interpret_staged_%s_%s"
                         program_name arg;
                     body =
                       Printf.sprintf "let box u = compiled in u with (%s);;"
                         arg;
                     persist = false;
                   };
                 ])));
    };
  ]

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
