(*Benchmarks*)

open Core
open Lys_benchmarks
open Lys_benchmarks.Bench_defns

let benchmarks =
  [
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

    (* {
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
         name = "regexp";
         base_program_loc = "test/example_programs/regexp/regexp.lys";
         run = 200;
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
                (* ((12)|\e)3 *)
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
                (* (1|2)34 *)
                "Times (Plus (Const ('1'), Const ('2')), Times (Const ('3'), \
                 Const ('4')))";
                (* 0(a*b)* *)
                "Times (Const ('0'), Star (Times (Star (Const ('a')), \
                 Const('b'))))";
                (* 1** *)
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
       }; *)
    (* {
      base_program_loc = "test/example_programs/while_language/while.lys";
      run = 200;
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
    }; *)
    (* {
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
       }; *)
  ]

let benchmarks2 =
  [
    (* Expected 15 min*)
    (* {
         base_program_loc = "test/example_programs/simple_programs/hello_world.lys";
         run = 2000;
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
             (* (2, [ 1; 2; 4; 8; 16; 32; 64 ]);
             (4, [ 1; 2; 4; 8; 16; 32; 64 ]);
             (8, [ 1; 2; 4; 8; 16; 32; 64 ]); *)
             (16, [ 1; (* 2; 4; 8; 16; 32; 64 *) ]);
             (* (32, [ 1; 2; 4; 8; 16; 32; 64 ]);
             (64, [ 1; 2; 4; 8; 16; 32; 64 ]); *)
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
       }; *)
    (*1.25h*)
    (* {
      name = "regexp_0(a*b)*_accept";
      base_program_loc = "test/example_programs/regexp/regexp.lys";
      run = 1000;
      program =
        {
          program_run = "accept1";
          program_compile = "accept2";
          program_staged_name = "p";
          compiled_type = "[str: string]bool";
        };
      arguments =
        (let generate_random_string_accept length =
           let rec generate_random_string_aux length =
             if length = 0 then ""
             else
               let random_number = Random.int ((length / 2) + 1) in
               String.init random_number ~f:(const 'a')
               ^ "b"
               ^ generate_random_string_aux (length - random_number - 1)
           in
           "0" ^ generate_random_string_aux (length - 1)
         in
         let regexps =
           [
             (* 0(a*b)* *)
             "Times (Const ('0'), Star (Times (Star (Const ('a')), \
              Const('b'))))";
           ]
         in
         let strs =
           List.init 5 ~f:(fun i ->
               generate_random_string_accept ((i + 1) * 20))
         in
         let mapped_strs =
           List.mapi strs ~f:(fun _ s ->
               { name = Some s; body = Printf.sprintf "\"%s\"" s })
         in
         List.mapi
           ~f:(fun i regexp ->
             ({ name = Some (Int.to_string i); body = regexp }, mapped_strs))
           regexps);
    };
    (*2 min*)
    {
      name = "regexp_0(a*b)*_reject";
      base_program_loc = "test/example_programs/regexp/regexp.lys";
      run = 1000;
      program =
        {
          program_run = "accept1";
          program_compile = "accept2";
          program_staged_name = "p";
          compiled_type = "[str: string]bool";
        };
      arguments =
        (let generate_random_string_reject length =
           "0"
           ^ String.init (length - 1) ~f:(fun _ ->
                 let i = Random.int 3 in
                 match i with
                 | 0 -> 'a'
                 | 1 -> 'b'
                 | 2 -> 'c'
                 | _ -> failwith "should never reach here")
           (*Have big proba for long run*)
         in
         let regexps =
           [
             (* 0(a*b)* *)
             "Times (Const ('0'), Star (Times (Star (Const ('a')), \
              Const('b'))))";
           ]
         in
         let strs =
           List.init 5 ~f:(fun i ->
               generate_random_string_reject ((i + 1) * 20))
         in
         let mapped_strs =
           List.mapi strs ~f:(fun _ s ->
               { name = Some s; body = Printf.sprintf "\"%s\"" s })
         in
         List.mapi
           ~f:(fun i regexp ->
             ({ name = Some (Int.to_string i); body = regexp }, mapped_strs))
           regexps);
    }; *)
    (* {
      name = "regexp_1**_accept";
      base_program_loc = "test/example_programs/regexp/regexp.lys";
      run = 1000;
      program =
        {
          program_run = "accept1";
          program_compile = "accept2";
          program_staged_name = "p";
          compiled_type = "[str: string]bool";
        };
      arguments =
        (let generate_random_string_accept length =
           String.init ~f:(const '1') length
         in
         let regexps = [ "Star (Star (Const ('1')))" ] in
         let strs =
           List.init 5 ~f:(fun i ->
               generate_random_string_accept ((i + 1) * 20))
         in
         let mapped_strs =
           List.mapi strs ~f:(fun _ s ->
               { name = Some s; body = Printf.sprintf "\"%s\"" s })
         in
         List.mapi
           ~f:(fun i regexp ->
             ({ name = Some (Int.to_string i); body = regexp }, mapped_strs))
           regexps);
    };
    {
      name = "regexp_1**_reject";
      base_program_loc = "test/example_programs/regexp/regexp.lys";
      run = 1000;
      program =
        {
          program_run = "accept1";
          program_compile = "accept2";
          program_staged_name = "p";
          compiled_type = "[str: string]bool";
        };
      arguments =
        (let generate_random_string_reject length =
           String.init (length - 1) ~f:(fun _ ->
               if Random.int 3 < 2 then '1' else '0')
           (*Bigger proba for long run*)
         in
         let regexps = [ "Star (Star (Const ('1')))" ] in
         let strs =
           List.init 5 ~f:(fun i ->
               generate_random_string_reject ((i + 1) * 20))
         in
         let mapped_strs =
           List.mapi strs ~f:(fun _ s ->
               { name = Some s; body = Printf.sprintf "\"%s\"" s })
         in
         List.mapi
           ~f:(fun i regexp ->
             ({ name = Some (Int.to_string i); body = regexp }, mapped_strs))
           regexps);
    }; *)
    {
      name = "regexp_(aa|aa)*_exp_runtime";
      base_program_loc = "test/example_programs/regexp/regexp.lys";
      run = 100;
      program =
        {
          program_run = "accept1";
          program_compile = "accept2";
          program_staged_name = "p";
          compiled_type = "[str: string]bool";
        };
      arguments =
        (let generate_random_string length =
           String.init ~f:(const 'a') length
         in
         let regexps = [ "Star (Plus (Times (Const ('a'), Const ('a')), Times (Const ('a'), Const ('a'))))" ] in
         let strs =
           List.init 10 ~f:(fun i ->
               generate_random_string ((2 * i + 1)))
         in
         let mapped_strs =
           List.mapi strs ~f:(fun _ s ->
               { name = Some s; body = Printf.sprintf "\"%s\"" s })
         in
         List.mapi
           ~f:(fun i regexp ->
             ({ name = Some (Int.to_string i); body = regexp }, mapped_strs))
           regexps);
    };
  ]

let (additional_benchmarks2 : base_benchmark_record_legacy list) =
  [
    (*Expected 1h*)
    {
      base_program_loc = "test/example_programs/simple_programs/hello_world.lys";
      run = 1000;
      benchmarks =
        List.map
          [
            (* (2, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]);
               (4, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]);
               (8, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]); *)
            (16, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]);
            (* (32, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]);
               (64, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]); *)
          ]
          ~f:(fun (exponent, length_list) ->
            List.map length_list ~f:(fun length ->
                let random_list = List.init length ~f:(fun _ -> 0) in
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
                         in fun (x: int) -> u with (x) in\n\
                        \                         map_int_int (pow_compiled) \
                         (%s);;"
                        exponent xs;
                  };
                ])
            |> List.concat)
        |> List.concat;
    };
    (* {
         base_program_loc = "test/example_programs/simple_programs/hello_world.lys";
         run = 300;
         benchmarks =
           List.map
             [
               (* (2, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]);
               (4, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]);
               (8, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]); *)
               (16, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]);
               (* (32, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]);
               (64, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512 ]); *)
             ]
             ~f:(fun (exponent, length_list) ->
               List.map length_list ~f:(fun length ->
                 let random_list =
                   List.init length ~f:(fun _ -> 0)
                 in
                 let xs =
                   Benchmark_utils.print_int_list random_list "Cons" "Nil"
                 in
                   [
                     {
                       persist = false;
                       name = Printf.sprintf "iter_pow_%i_%i_run" exponent length;
                       body =
                         Printf.sprintf "run_n_times (pow (%i)) (%i) (%s);;" exponent length xs;
                     };
                     {
                       persist = false;
                       name = Printf.sprintf "iter_pow_%i_%i_staged" exponent length;
                       body =
                         Printf.sprintf
                           "let pow_compiled: int -> int = let f:[x: int]int = pow2 (%i) \
                            in fun (x: int) -> let box u = f in u with (x) in\n\
                           \                         run_n_times (pow_compiled) (%i) (%s);;"
                           exponent length xs;
                     };
                   ])
               |> List.concat)
           |> List.concat;
       }; *)
  ]

let () =
  Command.basic_spec ~summary:"Run Benchmarks and save to file."
    Command.Spec.(
      empty
      +> flag "save" (optional string) ~aliases:[ "s" ] ~doc:"Save to file")
    (fun filename_opt () ->
      let bench =
        (* Bench_cb.compile_bench benchmarks2 *)
        Bench_cb.compile_bench benchmarks
        (* Bench_cb.compile_bench benchmarks *)
        (* @ Bench_cb.compile_bench_legacy additional_benchmarks2 *)
      in
      match filename_opt with
      | None ->
          bench |> Bench_cb.run_bench_exn
          |> Core_bench.Bench.display
               ~display_config:Bench_cb.default_display_config
      | Some filename ->
          bench |> Bench_cb.run_and_translate_to_csv |> Csv.save filename)
  |> Command_unix.run
(* let random_list =
     List.init 512 ~f:(fun _ -> Random.int Int.max_value)
   in
   let xs =
     Benchmark_utils.print_int_list random_list "Cons" "Nil"
   in
   print_endline xs *)
