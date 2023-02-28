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

let benchmarks2 =
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
          (10, [ 1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024; 2048; 4096; 8192 ]);
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
      name = "regexp_0(a*b)*_accept";
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
           List.init 10 ~f:(fun i -> generate_random_string_accept (i + 1))
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
      name = "regexp_0(a*b)*_reject";
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
           List.init 10 ~f:(fun i -> generate_random_string_reject (i + 1))
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
      name = "regexp_1**_accept";
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
        (let generate_random_string_accept length =
           String.init ~f:(const '1') length
         in
         let regexps = [ "Star (Star (Const ('1')))" ] in
         let strs =
           List.init 10 ~f:(fun i -> generate_random_string_accept (i + 1))
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
      run = 100;
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
           List.init 10 ~f:(fun i -> generate_random_string_reject (i + 1))
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

let () =
  Command.basic_spec ~summary:"Run Benchmarks and save to file."
    Command.Spec.(
      empty
      +> flag "save" (optional string) ~aliases:[ "s" ] ~doc:"Save to file")
    (fun filename_opt () ->
      match filename_opt with
      | None -> Bench_cb.bench_display_exn benchmarks2
      | Some filename ->
          Bench_cb.bench_to_csv_exn benchmarks2 |> Csv.save filename)
  |> Command_unix.run
