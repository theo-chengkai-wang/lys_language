open Core
open OUnit2
open Lys_ast
open Lys_typing

let read_parse_file_as_program filename =
  In_channel.with_file filename ~f:(fun file_ic ->
      Lys_parsing.Lex_and_parse.parse_program (Lexing.from_channel file_ic))

exception ParseFailed

let parse_expr_exn lexbuf =
  match Lys_parsing.Lex_and_parse.parse_expression lexbuf with
  | None -> raise ParseFailed
  | Some e -> e

let type_infer
    ?meta_context:(meta_ctx =
        Typing_context.MetaTypingContext.create_empty_context ())
    ?obj_context:(obj_ctx =
        Typing_context.ObjTypingContext.create_empty_context ())
    ?type_context:(typ_ctx = Typing_context.TypeConstrTypingContext.empty)
    ?typevar_context:(typevar_ctx = Typing_context.PolyTypeVarContext.create_empty_context ())
    lexbuf =
  lexbuf |> parse_expr_exn |> Ast.Expr.of_past
  |> Lys_typing.Typecore.type_inference_expression meta_ctx obj_ctx typ_ctx typevar_ctx

let type_infer_from_str
    ?meta_context:(meta_ctx =
        Typing_context.MetaTypingContext.create_empty_context ())
    ?obj_context:(obj_ctx =
        Typing_context.ObjTypingContext.create_empty_context ())
    ?type_context:(typ_ctx = Typing_context.TypeConstrTypingContext.empty) str =
  type_infer ~meta_context:meta_ctx ~obj_context:obj_ctx ~type_context:typ_ctx
    (Lexing.from_string str)

let type_check_program_from_str
    ?meta_context:(meta_ctx =
        Typing_context.MetaTypingContext.create_empty_context ())
    ?obj_context:(obj_ctx =
        Typing_context.ObjTypingContext.create_empty_context ())
    ?type_context:(type_ctx = Typing_context.TypeConstrTypingContext.empty) str
    =
  str |> Lexing.from_string |> Lys_parsing.Lex_and_parse.parse_program
  |> Lys_ast.Ast.Program.of_past
  |> Lys_typing.Typecore.type_check_program ~meta_ctx ~obj_ctx ~type_ctx
  |> Or_error.ok

(* m1-m8 *)

let test_read_prog filename _ =
  let parsed_program =
    read_parse_file_as_program filename |> Ast.Program.of_past
  in
  match Typecore.type_check_program parsed_program with
  | Ok _ -> ()
  | Error _ -> assert_failure "Type checking failed."

let prefix =
  Filename.concat Filename.current_dir_name
    (Filename.concat "example_programs" "cmtt_paper_proofs")

let files_to_test =
  [
    "m1.lys";
    "m2.lys";
    "m3.lys";
    "m4.lys";
    "m5.lys";
    "m6.lys";
    "m7.lys";
    "m8.lys";
  ]

let example_programs_suite =
  "example_programs_suite"
  >::: List.map files_to_test ~f:(fun x ->
           "test_for_" ^ x >:: test_read_prog (Filename.concat prefix x))

(* Standard *)

let test_infer_integer _ =
  assert_equal (type_infer_from_str "1;;") (Ok Ast.Typ.TInt)

let test_both_sides_of_equality_must_have_same_type _ =
  assert_equal (Or_error.ok (type_infer_from_str "1=true;;")) None

let test_product_type_nth _ =
  assert_equal (Or_error.ok (type_infer_from_str "1[1];;")) None;
  assert_equal
    (Or_error.ok (type_infer_from_str "(1,1,1)[1];;"))
    (Some Ast.Typ.TInt)

let test_sum_type_inl _ =
  assert_equal
    (Or_error.ok (type_infer_from_str "L[int, bool] 1;;"))
    (Some (Ast.Typ.TSum (Ast.Typ.TInt, Ast.Typ.TBool)));
  assert_equal (Or_error.ok (type_infer_from_str "L[int, bool] true;;")) None

let test_sum_type_inr _ =
  assert_equal
    (Or_error.ok (type_infer_from_str "R[int, bool] true;;"))
    (Some (Ast.Typ.TSum (Ast.Typ.TInt, Ast.Typ.TBool)));
  assert_equal (Or_error.ok (type_infer_from_str "R[int, bool] 1;;")) None

let test_case _ =
  assert_equal
    (Or_error.ok
       (type_infer_from_str
          "case (L[int, int] 1) of L (y:int) -> y | R (z:int) -> z;;"))
    (Some Ast.Typ.TInt);
  assert_equal
    (Or_error.ok
       (type_infer_from_str
          "case (L[int, int] true) of L (y:int) -> y | R (z:int) -> z;;"))
    None;
  assert_equal
    (Or_error.ok
       (type_infer_from_str
          "case (L[int, int] 1) of L (y:int) -> true | R (z:int) -> z;;"))
    None

let test_application_only_functions _ =
  assert_equal (Or_error.ok (type_infer_from_str "1 2;;")) None;
  assert_equal
    (Or_error.ok (type_infer_from_str "(fun (x:int) -> x) 1;;"))
    (Some Ast.Typ.TInt)

let test_if_then_else _ =
  assert_equal
    (Or_error.ok (type_infer_from_str "if true then 1 else 2;;"))
    (Some Ast.Typ.TInt);
  assert_equal (Or_error.ok (type_infer_from_str "if 1 then 1 else 2;;")) None;
  assert_equal
    (Or_error.ok (type_infer_from_str "if true then 1 else true;;"))
    None

let test_let_rec _ =
  assert_equal
    (Or_error.ok
       (type_infer_from_str
          "let rec pow:int->int->int = fun (n:int) -> fun (x:int) -> if n = 0 \
           then 1 else x * (pow (n-1) x) in ();;"))
    (Some Ast.Typ.TUnit);
  assert_equal ~msg:"problem with rec functions must have function type"
    (Or_error.ok (type_infer_from_str "let rec x:int = x in ();;"))
    None

let test_let_rec_mut _ =
  assert_equal (Some Ast.Typ.TBool)
    (Or_error.ok
       (type_infer_from_str
          "let rec even: int -> bool = \n\
          \      fun (n:int) ->\n\
          \          if (n = 0) then true else odd (n - 1)\n\
          \  and\n\
          \  odd: int -> bool =\n\
          \      fun (n:int) -> if (n=0) then false else even (n-1)\n\
          \  in\n\
          \  even (1);;"))

let test_rec_mut_fails _ =
  assert_equal None
    (Or_error.ok
       (type_infer_from_str
          "let rec even: int -> int = \n\
          \    fun (n:int) ->\n\
          \        if (n = 0) then true else odd (n - 1)\n\
           and\n\
           odd: int -> bool =\n\
          \    fun (n:int) -> if (n=0) then false else even (n-1)\n\
           in\n\
           even (1);;"))

let test_bound_identifier _ =
  assert_equal
    (Or_error.ok
       (type_infer_from_str
          ~obj_context:
            ( () |> Typing_context.ObjTypingContext.create_empty_context
            |> fun ctx ->
              Typing_context.ObjTypingContext.add_mapping ctx
                (Ast.ObjIdentifier.of_string "x")
                Ast.Typ.TInt )
          "x;;"))
    (Some Ast.Typ.TInt)

let test_unbound_identifier _ =
  assert_equal (Or_error.ok (type_infer_from_str "x;;")) None

let test_strings _ =
  assert_equal (Some Ast.Typ.TString)
    (Or_error.ok (type_infer_from_str "\"12345\";;"))

let test_string_match _ =
  let program =
    "\n\
    \    let x:string = \"123\";;\n\
    \    match x with\n\
    \    | a ++ as -> a\n\
    \    | \"\" -> 'd';;"
  in
  assert_bool "Type check hasn't failed"
    (Option.is_some (type_check_program_from_str program))

let standard_suite =
  "standard_suite"
  >::: [
         "test_infer_integer" >:: test_infer_integer;
         "test_both_sides_of_equality_must_have_same_type"
         >:: test_both_sides_of_equality_must_have_same_type;
         "test_product_type_nth" >:: test_product_type_nth;
         "test_sum_type_inl" >:: test_sum_type_inl;
         "test_sum_type_inr" >:: test_sum_type_inr;
         "test_case" >:: test_case;
         "test_application_only_functions" >:: test_application_only_functions;
         "test_if_then_else" >:: test_if_then_else;
         "test_let_rec" >:: test_let_rec;
         "test_let_rec_mut" >:: test_let_rec_mut;
         "test_rec_mut_fails" >:: test_rec_mut_fails;
         "test_bound_identifier" >:: test_bound_identifier;
         "test_unbound_identifier" >:: test_unbound_identifier;
         "test_strings" >:: test_strings;
         "test_string_match" >:: test_string_match;
       ]

(* CMTT *)

let test_box _ =
  assert_equal
    (Or_error.ok (type_infer_from_str "box (x:int |- x);;"))
    (Some (TBox ([ (Ast.ObjIdentifier.of_string "x", TInt) ], TInt)));
  assert_equal ~msg:"Duplicate variable in box context"
    (Or_error.ok (type_infer_from_str "box (x: int, x:int |- x);;"))
    None

let test_let_box _ =
  assert_equal
    (Or_error.ok
       (type_infer_from_str "let box u = box (x:int |- x) in (u with (1));;"))
    (Some TInt);
  assert_equal ~msg:"problem with can't let box a normal term"
    (Or_error.ok (type_infer_from_str "let box u = 1 in ();;"))
    None

let test_closure_correct _ =
  assert_equal
    (Or_error.ok
       (type_infer_from_str "let box u = box (x:int |- x) in (u with (1));;"))
    (Some TInt)

let test_closure_unbound_meta _ =
  assert_equal (Or_error.ok (type_infer_from_str "(u with (1));;")) None

let test_closure_unmatched_context_and_arg_list_length _ =
  assert_equal
    (Or_error.ok
       (type_infer_from_str "let box u = box (x:int |- x) in (u with (1, 2));;"))
    None

let test_closure_unmatched_context_wrt_arguments _ =
  assert_equal
    (Or_error.ok
       (type_infer_from_str "let box u = box (x:int |- x) in (u with (true));;"))
    None

let test_lift_primitive _ =
  assert_equal
    (Some (Ast.Typ.TBox ([], Ast.Typ.TInt)))
    (Or_error.ok (type_infer_from_str "lift[int] 1;;"))

let test_lift_non_primitive _ =
  let program =
    "datatype tree = Lf | Br of (int * tree * tree);;\n\
    \  let t:tree = Br (1, Lf, Br (2, Br (3, Lf, Lf), Lf));;"
  in
  assert_bool "Program type checks"
    (type_check_program_from_str program |> Option.is_some)

let test_lift_function_fails _ =
  assert_equal None
    ("lift[int -> int] (fun (x:int) -> x);;" |> type_infer_from_str
   |> Or_error.ok)

let cmtt_suite =
  "cmtt_suite"
  >::: [
         "test_box" >:: test_box;
         "test_let_box_correct" >:: test_let_box;
         "test_closure_correct" >:: test_closure_correct;
         "test_closure_unbound_meta" >:: test_closure_unbound_meta;
         "test_closure_unmatched_context_and_arg_list_length"
         >:: test_closure_unmatched_context_and_arg_list_length;
         "test_closure_unmatched_context_wrt_arguments"
         >:: test_closure_unmatched_context_wrt_arguments;
         "test_lift_primitive" >:: test_lift_primitive;
         "test_lift_non_primitive" >:: test_lift_non_primitive;
         "test_lift_function_fails" >:: test_lift_function_fails;
       ]

(*ADT Suite*)
let test_datatypes _ =
  let program =
    "\n\
    \  datatype sometype = Con1 of int | Con3 of unit | Con4 of (int * \
     sometype);;\n\n\
    \  let x:sometype = Con1 1;;\n\
    \  \n\
    \  let y:sometype = Con4 (1, Con1 1);;\n\
    \  \n\
    \  match y with\n\
    \      | Con1 (x) -> x\n\
    \      | Con3 (u) -> 2\n\
    \      | Con4 (i, s) ->  i\n\
    \      | _ -> 1;;\n\
    \  "
  in
  assert_bool "Type check hasn't failed"
    (Option.is_some (type_check_program_from_str program))

let test_datatypes_mutually_recursive _ =
  let program =
    "datatype sometype = A of int | B of sometype | C of (othertype)\n\
    \  and othertype = D | E of (sometype);;\n\
    \  \n\
    \  A (1);;\n\
    \  \n\
    \  B (A (1));;\n\
    \  \n\
    \  C (D);;\n\
    \  \n\
    \  E (C (D));;\n\
    \  "
  in
  assert_bool "Type check hasn't failed"
    (Option.is_some (type_check_program_from_str program))

let adt_suite =
  "adt_suite"
  >::: [
         "test_datatypes" >:: test_datatypes;
         "test_datatypes_mutually_recursive"
         >:: test_datatypes_mutually_recursive;
       ]

let test_seq_refs _ =
  let program =
    "let a: unit = ();;\n\
    \    let b: int ref = ref 123;;\n\
    \    a; b:= 1; !b;;\n\
    \    b;;"
  in
  assert_bool "Type check hasn't failed"
    (Option.is_some (type_check_program_from_str program))

let test_ref_type _ =
  assert_equal
    (Or_error.ok (type_infer_from_str "ref 123;;"))
    (Some (TRef TInt))

let test_deref_type _ =
  assert_equal (Or_error.ok (type_infer_from_str "!(ref 123);;")) (Some TInt)

let test_seq_type _ =
  assert_equal (Or_error.ok (type_infer_from_str "();();1;;")) (Some TInt)

let test_seq_type_wrong _ =
  assert_equal (Or_error.ok (type_infer_from_str "();1;1;;")) None

let test_while_with_fib _ =
  assert_equal
    (Some (Ast.Typ.TFun (Ast.Typ.TInt, Ast.Typ.TInt)))
    (Or_error.ok
       (type_infer_from_str
          "\n\
          \  fun (n:int) ->\n\
          \    if n <= 1 then n else\n\
          \    let fib_n_2: int ref = ref 0 in\n\
          \    let fib_n_1: int ref = ref 1 in\n\
          \    let counter: int ref = ref 2 in\n\
          \    let tmp: int ref = ref 0 in \n\
          \    while (!counter <= n) do\n\
          \        tmp := !fib_n_1;\n\
          \        fib_n_1 := !tmp + !fib_n_2;\n\
          \        fib_n_2 := !tmp;\n\
           counter := !counter + 1\n\
          \              done;\n\
          \    !fib_n_1;;\n"))

let test_array _ =
  assert_equal (Some (Ast.Typ.TArray Ast.Typ.TInt))
    (Or_error.ok (type_infer_from_str "[|1, 2, 3|];;"))

let test_array_assign _ =
  assert_equal (Some Ast.Typ.TUnit)
    (Or_error.ok (type_infer_from_str "([|1, 2, 3|]).(2) <- 123;;"))

let test_array_len _ =
  assert_equal (Some Ast.Typ.TInt)
    (Or_error.ok (type_infer_from_str "len ([|1, 2, 3|]);;"))

let test_array_index _ =
  assert_equal (Some Ast.Typ.TInt)
    (Or_error.ok (type_infer_from_str "([|1, 2, 3|]).(2);;"))

let imperative_suite =
  "imperative_suite"
  >::: [
         "test_seq_refs" >:: test_seq_refs;
         "test_ref_type" >:: test_ref_type;
         "test_seq_type" >:: test_seq_type;
         "test_seq_type_wrong" >:: test_seq_type_wrong;
         "test_deref_type" >:: test_deref_type;
         "test_while_with_fib" >:: test_while_with_fib;
         "test_array" >:: test_array;
         "test_array_assign" >:: test_array_assign;
         "test_array_len" >:: test_array_len;
         "test_array_index" >:: test_array_index;
       ]

let suite =
  "typing_suite"
  >::: [
         cmtt_suite;
         standard_suite;
         example_programs_suite;
         adt_suite;
         imperative_suite;
       ]
