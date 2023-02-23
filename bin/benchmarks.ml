open Core
open Lys_parsing
open Lys_typing
open Lys_interpreter
open Lys_ast

let evaluate_from_lexbuf_given_context interpreter lexbuf context typ_context =
  lexbuf |> Lex_and_parse.parse_program |> Ast.Program.of_past
  |> Typecore.type_check_program
       ~obj_ctx:
         (Interpreter_common.EvaluationContext.to_typing_obj_context context)
       ~type_ctx:
         (Interpreter_common.TypeConstrContext.to_typeconstrtypingcontext
            typ_context)
  |> ok_exn |> Ast.TypedProgram.populate_index |> ok_exn
  |> Interpreter.evaluate_top_level_defns ~top_level_context:context
       ~type_constr_context:typ_context ~interpreter
  |> ok_exn

(*Helper for convolution*)
let rec random_list len =
  if len = 0 then [] else Random.int 2048 :: random_list (len - 1)

(*Helper for turning int lists into*)
let rec print_int_list l cons nil =
  match l with
  | [] -> nil
  | x :: xs -> Printf.sprintf "%s (%i, %s)" cons x (print_int_list xs cons nil)

(*
  Accumulate a list of program paths zipped with a list of expr * int
  Execute that expr int times to get a list of such values
  (THINK CSV AGAIN PLEASE) -- maybe have record entry.
*)

type benchmark_program = { name : string; body : string; persist : bool }
[@@deriving show, sexp]

type base_benchmark_record = {
  base_program_loc : string;
  run : int;
  benchmarks : benchmark_program list;
}
[@@deriving show, sexp]

type benchmark_result_record = {
  base_program_loc : string;
  run_id : int;
  benchmark : benchmark_program;
  defn_id : int;
  steps : int;
  time : float;
}
[@@deriving show, sexp]

(*Print to CSV utils*)

let to_csv results =
  List.map results
    ~f:(fun { base_program_loc; run_id; benchmark; defn_id; steps; time } ->
      [
        base_program_loc;
        Int.to_string run_id;
        benchmark.name;
        Int.to_string defn_id;
        Int.to_string steps;
        Float.to_string time;
      ])
  |> fun l ->
  (*Header*)
  [ "base_program"; "run_id"; "benchmark"; "defn_id"; "steps"; "time" ] :: l

(*Step * Time list for each expression executed, and in a list.*)

let run_num = 100

let benchmarks =
  [
    {
      base_program_loc = "test/example_programs/simple_programs/hello_world.lys";
      run = run_num;
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
          (50, 2);
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
      run = run_num;
      benchmarks =
        [
          (random_list 5, random_list 5);
          (random_list 10, random_list 10);
          (random_list 100, random_list 100);
          (random_list 200, random_list 200);
        ]
        |> List.concat_map ~f:(fun (xs, ys) ->
               let i = List.length xs in
               let xs_str = print_int_list xs "Cons" "Nil" in
               let ys_str = print_int_list ys "Cons" "Nil" in
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
      run = run_num;
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
      run = run_num;
      benchmarks =
        (let programs_and_inputs =
           [
             ( "fib_program",
               [ 1; 2; 5; 10; 20; 50; 100 ]
               |> List.map ~f:(fun x_l ->
                      print_int_list [ x_l ] "Cons_i" "Nil_i") );
           ]
         in
         List.concat_map programs_and_inputs
           ~f:(fun (program_name, program_args) ->
             List.concat_map program_args ~f:(fun arg ->
                 [
                   {
                     name = Printf.sprintf "interpret_%s_%s" program_name arg;
                     body =
                       Printf.sprintf "interpret (%s) (%s);;" program_name arg;
                     persist = false;
                   };
                   {
                     name = Printf.sprintf "interpret_compile_%s" program_name;
                     body =
                       Printf.sprintf
                         "let compiled: [args: int_list]int_list = \
                          interpret_staged %s;;"
                         program_name;
                     persist = true;
                   };
                   {
                     name =
                       Printf.sprintf "interpret_staged_%s_%s" program_name arg;
                     body =
                       Printf.sprintf "let box u = compiled in u with (%s);;"
                         arg;
                     persist = false;
                   };
                 ])));
    };
    (* {
      base_program_loc = "test/example_programs/flowchart/flowchart.lys";
      run = run_num;
      benchmarks =
        (let programs_and_inputs =
           [
             ( "fib_program",
               [ 1; 2; 5; 10; 20; 50; 100 ]
               |> List.map ~f:(fun x_l ->
                      print_int_list [ x_l ] "Cons_i" "Nil_i") );
           ]
         in
         List.concat_map programs_and_inputs
           ~f:(fun (program_name, program_args) ->
             List.concat_map program_args ~f:(fun arg ->
                 [
                   {
                     name = Printf.sprintf "interpret_%s_%s" program_name arg;
                     body =
                       Printf.sprintf "interpret (%s) (%s);;" program_name arg;
                     persist = false;
                   };
                   {
                     name = Printf.sprintf "interpret_compile_%s" program_name;
                     body =
                       Printf.sprintf
                         "let compiled: [args: int_list]int = \
                          interpret_staged %s;;"
                         program_name;
                     persist = true;
                   };
                   {
                     name =
                       Printf.sprintf "interpret_staged_%s_%s" program_name arg;
                     body =
                       Printf.sprintf "let box u = compiled in u with (%s);;"
                         arg;
                     persist = false;
                   };
                 ])));
    }; *)
  ]

(*TODO: Perform all benchmarks and convert to CSV*)

let get_top_level_non_rec_step_count_exn t =
  match t with
  | Interpreter_common.TopLevelEvaluationResult.Defn (_, _, _, Some v, _) -> v
  | Interpreter_common.TopLevelEvaluationResult.ExprValue (_, _, _, Some v, _)
    ->
      v
  | _ -> failwith "Not a valid top level evaluation result"

let get_top_level_non_rec_time_exn t =
  match t with
  | Interpreter_common.TopLevelEvaluationResult.Defn (_, _, Some v, _, _) -> v
  | Interpreter_common.TopLevelEvaluationResult.ExprValue (_, _, Some v, _, _)
    ->
      v
  | _ -> failwith "Not a valid top level evaluation result"

let rec perform_run run_cnt base_program_loc { name; body; persist } eval_ctx
    typ_ctx run_id =
  print_endline
    ("... ... benchmarking: " ^ name ^ "; run: " ^ Int.to_string run_id);
  (*Performs run_cnt number of runs and get both the step cnt and the time measurement*)
  if run_id >= run_cnt then
    (*Compute new context if we have persist = true*)
    if persist then
      let _, new_eval_ctx, new_typ_context =
        evaluate_from_lexbuf_given_context
          (Interpreter.MultiStep { show_time = false })
          (Lexing.from_string body) eval_ctx typ_ctx
      in
      ([], new_eval_ctx, new_typ_context)
    else ([], eval_ctx, typ_ctx)
  else
    (*Step*)
    let top_level_results, _, _ =
      evaluate_from_lexbuf_given_context
        (Interpreter.SingleStep { show_step_count = true; verbose = false })
        (Lexing.from_string body) eval_ctx typ_ctx
    in
    let results_step =
      List.map top_level_results ~f:(fun top_level ->
          get_top_level_non_rec_step_count_exn top_level)
    in
    (*Time*)
    let top_level_results, _, _ =
      evaluate_from_lexbuf_given_context
        (Interpreter.MultiStep { show_time = true })
        (Lexing.from_string body) eval_ctx typ_ctx
    in
    let results_time =
      List.map top_level_results ~f:(fun top_level ->
          get_top_level_non_rec_time_exn top_level)
    in
    List.zip_exn results_step results_time
    |> List.mapi ~f:(fun i (steps, time) ->
           {
             base_program_loc;
             run_id;
             benchmark = { name; body; persist };
             defn_id = i;
             steps;
             time;
           })
    |> fun res ->
    let rest, new_eval_ctx, new_typ_context =
      perform_run run_cnt base_program_loc { name; body; persist } eval_ctx
        typ_ctx (run_id + 1)
    in
    (res @ rest, new_eval_ctx, new_typ_context)

let do_benchmark ({ base_program_loc; run; benchmarks } : base_benchmark_record)
    =
  (*do a benchmark*)
  print_endline
    (Printf.sprintf "---------------------- \n Fetching: %s" base_program_loc);
  let _, eval_ctx, typ_ctx =
    In_channel.with_file base_program_loc ~f:(fun file_ic ->
        evaluate_from_lexbuf_given_context
          (Interpreter.MultiStep { show_time = false })
          (Lexing.from_channel file_ic)
          Interpreter_common.EvaluationContext.empty
          Interpreter_common.TypeConstrContext.empty)
  in
  List.fold benchmarks ~init:([], eval_ctx, typ_ctx)
    ~f:(fun (current_list, eval_ctx, typ_ctx) bm ->
      print_endline ("... benchmarking " ^ bm.name);
      perform_run run base_program_loc bm eval_ctx typ_ctx 0
      |> fun (results, new_eval_ctx, new_typ_ctx) ->
      (current_list @ results, new_eval_ctx, new_typ_ctx))
  |> fun (results, _, _) -> results

let perform_all_benchmarks bms = List.concat_map bms ~f:do_benchmark

let () =
  perform_all_benchmarks benchmarks
  |> to_csv
  |> Csv.save "traces/benchmarks/20230222_benchmarks_except_flowchart.csv"
(* random_list 200
   |> (fun l -> print_int_list l "Cons" "Nil") |>
   print_endline *)
