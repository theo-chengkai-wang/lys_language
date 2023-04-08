open OUnit2
open Lys_parsing
open Lys_typing
open Lys_ast
open Lys_interpreter
open Core

let exec_program interpreter str =
  str |> Lexing.from_string |> Lex_and_parse.parse_program
  |> Ast.Program.of_past |> Typecore.type_check_program |> ok_exn
  |> Ast.TypedProgram.populate_index |> ok_exn
  |> Interpreter.evaluate_program ~interpreter
  |> Or_error.ok

let print_res res =
  List.iter res ~f:(fun res ->
      print_endline (Interpreter_common.TopLevelEvaluationResult.show res);
      print_endline "")

let create_debruijn_exn n = n |> Ast.DeBruijnIndex.create |> ok_exn

let generate_tests_for_interpreter interpreter =
  let test_interp_int _ =
    let res_opt = exec_program interpreter "1;;" in
    assert_equal
      (Some
         [
           Interpreter_common.TopLevelEvaluationResult.ExprValue
             ( Ast.Typ.TInt,
               Ast.Value.Constant (Ast.Constant.Integer 1),
               None,
               None,
               None );
         ])
      res_opt
  in
  let test_interp_var_def _ =
    let program = "let x:int = 0;; x;;" in
    let res_opt = exec_program interpreter program in
    (* print_res (Option.value_exn res_opt); *)
    assert_equal
      (Some
         [
           Interpreter_common.TopLevelEvaluationResult.Defn
             ( ( Ast.ObjIdentifier.of_string_and_index "x" Ast.DeBruijnIndex.none,
                 Ast.Typ.TInt ),
               Ast.Value.Constant (Ast.Constant.Integer 0),
               None,
               None,
               None );
           Interpreter_common.TopLevelEvaluationResult.ExprValue
             ( Ast.Typ.TInt,
               Ast.Value.Constant (Ast.Constant.Integer 0),
               None,
               None,
               None );
         ])
      res_opt
  in
  let test_interp_rec_var_def _ =
    let program =
      "let rec (pow: int -> int -> int) = fun (n:int) -> if n = 0 then (fun \
       (x:int) -> 1) else (fun (x:int) -> x * (pow (n-1) x));; pow 2 3;;"
    in
    let res_opt = exec_program interpreter program in
    (* print_res (Option.value_exn res_opt); *)
    assert_equal
      (Some
         [
           Interpreter_common.TopLevelEvaluationResult.RecDefn
             ( ( Ast.ObjIdentifier.of_string_and_index "pow"
                   Ast.DeBruijnIndex.none,
                 Ast.Typ.TFun
                   (Ast.Typ.TInt, Ast.Typ.TFun (Ast.Typ.TInt, Ast.Typ.TInt)) ),
               Ast.Value.Lambda
                 ( ( Ast.ObjIdentifier.of_string_and_index "n"
                       Ast.DeBruijnIndex.none,
                     Ast.Typ.TInt ),
                   Ast.Expr.IfThenElse
                     ( Ast.Expr.BinaryOp
                         ( Ast.BinaryOperator.EQ,
                           Ast.Expr.Identifier
                             (Ast.ObjIdentifier.of_string_and_index "n"
                                (create_debruijn_exn 0)),
                           Ast.Expr.Constant (Ast.Constant.Integer 0) ),
                       Ast.Expr.Lambda
                         ( ( Ast.ObjIdentifier.of_string_and_index "x"
                               Ast.DeBruijnIndex.none,
                             Ast.Typ.TInt ),
                           Ast.Expr.Constant (Ast.Constant.Integer 1) ),
                       Ast.Expr.Lambda
                         ( ( Ast.ObjIdentifier.of_string_and_index "x"
                               Ast.DeBruijnIndex.none,
                             Ast.Typ.TInt ),
                           Ast.Expr.BinaryOp
                             ( Ast.BinaryOperator.MUL,
                               Ast.Expr.Identifier
                                 (Ast.ObjIdentifier.of_string_and_index "x"
                                    (create_debruijn_exn 0)),
                               Ast.Expr.Application
                                 ( Ast.Expr.Application
                                     ( Ast.Expr.Identifier
                                         (Ast.ObjIdentifier.of_string_and_index
                                            "pow" (create_debruijn_exn 2)),
                                       Ast.Expr.BinaryOp
                                         ( Ast.BinaryOperator.SUB,
                                           Ast.Expr.Identifier
                                             (Ast.ObjIdentifier
                                              .of_string_and_index "n"
                                                (create_debruijn_exn 1)),
                                           Ast.Expr.Constant
                                             (Ast.Constant.Integer 1) ) ),
                                   Ast.Expr.Identifier
                                     (Ast.ObjIdentifier.of_string_and_index "x"
                                        (create_debruijn_exn 0)) ) ) ) ) ),
               None,
               None,
               None );
           Interpreter_common.TopLevelEvaluationResult.ExprValue
             ( Ast.Typ.TInt,
               Ast.Value.Constant (Ast.Constant.Integer 9),
               None,
               None,
               None );
         ])
      res_opt
  in
  let test_interp_expr _ =
    let program = "5+5;;" in
    let res_opt = exec_program interpreter program in
    assert_equal
      (Some
         [
           Interpreter_common.TopLevelEvaluationResult.ExprValue
             ( Ast.Typ.TInt,
               Ast.Value.Constant (Ast.Constant.Integer 10),
               None,
               None,
               None );
         ])
      res_opt
  in
  let test_staging_pow _ =
    let program =
      "let rec (pow: int -> [b:int]int) = fun (n:int) -> if n = 0 then box \n\
      \      (b:int |- 1) else let box u = pow (n-1) in box (b:int |- b * (u \
       with (b)));;\n\
      \     pow 2;;\n\
      \     let box u = pow 2 in u with (3);;"
    in
    let res_opt = exec_program interpreter program in
    (* print_res (Option.value_exn res_opt); *)
    assert_equal
      (Some
         [
           Interpreter_common.TopLevelEvaluationResult.RecDefn
             ( ( Ast.ObjIdentifier.of_string_and_index "pow"
                   Ast.DeBruijnIndex.none,
                 Ast.Typ.TFun
                   ( Ast.Typ.TInt,
                     Ast.Typ.TBox
                       ( [ (Ast.ObjIdentifier.of_string "b", Ast.Typ.TInt) ],
                         Ast.Typ.TInt ) ) ),
               Ast.Value.Lambda
                 ( (Ast.ObjIdentifier.of_string "n", Ast.Typ.TInt),
                   Ast.Expr.IfThenElse
                     ( Ast.Expr.BinaryOp
                         ( Ast.BinaryOperator.EQ,
                           Ast.Expr.Identifier
                             (Ast.ObjIdentifier.of_string_and_index "n"
                                (create_debruijn_exn 0)),
                           Ast.Expr.Constant (Ast.Constant.Integer 0) ),
                       Ast.Expr.Box
                         ( [ (Ast.ObjIdentifier.of_string "b", Ast.Typ.TInt) ],
                           Ast.Expr.Constant (Ast.Constant.Integer 1) ),
                       Ast.Expr.LetBox
                         ( Ast.MetaIdentifier.of_string "u",
                           Ast.Expr.Application
                             ( Ast.Expr.Identifier
                                 (Ast.ObjIdentifier.of_string_and_index "pow"
                                    (create_debruijn_exn 1)),
                               Ast.Expr.BinaryOp
                                 ( Ast.BinaryOperator.SUB,
                                   Ast.Expr.Identifier
                                     (Ast.ObjIdentifier.of_string_and_index "n"
                                        (create_debruijn_exn 0)),
                                   Ast.Expr.Constant (Ast.Constant.Integer 1) )
                             ),
                           Ast.Expr.Box
                             ( [
                                 (Ast.ObjIdentifier.of_string "b", Ast.Typ.TInt);
                               ],
                               Ast.Expr.BinaryOp
                                 ( Ast.BinaryOperator.MUL,
                                   Ast.Expr.Identifier
                                     (Ast.ObjIdentifier.of_string_and_index "b"
                                        (create_debruijn_exn 0)),
                                   Ast.Expr.Closure
                                     ( Ast.MetaIdentifier.of_string_and_index
                                         "u" (create_debruijn_exn 0),
                                       [
                                         Ast.Expr.Identifier
                                           (Ast.ObjIdentifier
                                            .of_string_and_index "b"
                                              (create_debruijn_exn 0));
                                       ] ) ) ) ) ) ),
               None,
               None,
               None );
           Interpreter_common.TopLevelEvaluationResult.ExprValue
             ( Ast.Typ.TBox
                 ( [ (Ast.ObjIdentifier.of_string "b", Ast.Typ.TInt) ],
                   Ast.Typ.TInt ),
               Ast.Value.Box
                 ( [ (Ast.ObjIdentifier.of_string "b", Ast.Typ.TInt) ],
                   Ast.Expr.BinaryOp
                     ( Ast.BinaryOperator.MUL,
                       Ast.Expr.Identifier
                         (Ast.ObjIdentifier.of_string_and_index "b"
                            (create_debruijn_exn 0)),
                       Ast.Expr.BinaryOp
                         ( Ast.BinaryOperator.MUL,
                           Ast.Expr.Identifier
                             (Ast.ObjIdentifier.of_string_and_index "b"
                                (create_debruijn_exn 0)),
                           Ast.Expr.Constant (Ast.Constant.Integer 1) ) ) ),
               None,
               None,
               None );
           Interpreter_common.TopLevelEvaluationResult.ExprValue
             ( Ast.Typ.TInt,
               Ast.Value.Constant (Ast.Constant.Integer 9),
               None,
               None,
               None );
         ])
      res_opt
  in
  let test_datatype_matching _ =
    let program =
      "datatype tree = Lf | Br of (int * tree * tree);;\n\n\
      \    let t2: tree = Br (1, Br (3, Lf, Lf), Br (4, Lf, Lf));;\n\
      \    let rec sum:(tree -> int) = fun (t:tree) -> \n\
      \      match t with\n\
      \      | Lf -> 0 \n\
      \      | Br (i, l, r) -> i + (sum l) + (sum r);;\n\
      \    sum t2;;"
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TInt,
                  Ast.Value.Constant (Ast.Constant.Integer 8),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_matching_n_ary_product _ =
    let program =
      "let x4: (int * unit * (unit * int) * int) = (1, (), ((), 2), 3);;\n\n\
      \    match x4 with\n\
      \    | (a, b, c, d) -> c;;"
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TProd [ Ast.Typ.TUnit; Ast.Typ.TInt ],
                  Ast.Value.Prod
                    [
                      Ast.Value.Constant Ast.Constant.Unit;
                      Ast.Value.Constant (Ast.Constant.Integer 2);
                    ],
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_matching_inl_inr _ =
    let program =
      "match (L[int, unit] 10) with\n\
       | L x -> x\n\
       | R y -> 0;;\n\n\
       match (R[int, unit] ()) with\n\
       | L x -> x\n\
       | R y -> 0;;"
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          [
            Interpreter_common.TopLevelEvaluationResult.ExprValue
              ( Ast.Typ.TInt,
                Ast.Value.Constant (Ast.Constant.Integer 10),
                None,
                None,
                None );
            Interpreter_common.TopLevelEvaluationResult.ExprValue
              ( Ast.Typ.TInt,
                Ast.Value.Constant (Ast.Constant.Integer 0),
                None,
                None,
                None );
          ]
          results
  in
  let test_match_wildcard _ =
    let program =
      "datatype sometype = Con1 of int | Con2 | Con3 of unit | Con4 of (int * \
       sometype);;\n\
      \    let y:sometype = Con2;;\n\
      \    \n\
      \    match y with\n\
      \        | Con1 (x) -> 1\n\
      \        | Con3 (u) -> 2\n\
      \        | _ -> 0;;"
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TInt,
                  Ast.Value.Constant (Ast.Constant.Integer 0),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_match_identifier _ =
    let program =
      "datatype sometype = Con1 of int | Con2 | Con3 of unit | Con4 of (int * \
       sometype);;\n\
      \            let y:sometype = Con2;;\n\
      \            \n\
      \            match y with\n\
      \                | Con1 (x) -> Con1 1\n\
      \                | Con3 (u) -> Con1 1\n\
      \                | z -> z;;"
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "sometype"),
                  Ast.Value.Constr (Ast.Constructor.of_string "Con2", None),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_lift_primitive _ =
    let program = "lift[int] 1;;" in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TBox ([], Ast.Typ.TInt),
                  Ast.Value.Box ([], Ast.Expr.Constant (Ast.Constant.Integer 1)),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_lift_non_primitive _ =
    let program =
      "datatype tree = Lf | Br of (int * tree * tree);;\n\
      \  let t:tree = Br (1, Lf, Br (2, Lf, Lf));;\n\
      \  lift[tree] t;;"
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TBox
                    ( [],
                      Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "tree")
                    ),
                  Ast.Value.Box
                    ( [],
                      Ast.Expr.Constr
                        ( Ast.Constructor.of_string "Br",
                          Some
                            (Ast.Expr.Prod
                               [
                                 Ast.Expr.Constant (Ast.Constant.Integer 1);
                                 Ast.Expr.Constr
                                   (Ast.Constructor.of_string "Lf", None);
                                 Ast.Expr.Constr
                                   ( Ast.Constructor.of_string "Br",
                                     Some
                                       (Ast.Expr.Prod
                                          [
                                            Ast.Expr.Constant
                                              (Ast.Constant.Integer 2);
                                            Ast.Expr.Constr
                                              ( Ast.Constructor.of_string "Lf",
                                                None );
                                            Ast.Expr.Constr
                                              ( Ast.Constructor.of_string "Lf",
                                                None );
                                          ]) );
                               ]) ) ),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_mutual_recursive_expr _ =
    let program =
      "let rec even: int -> bool = \n\
      \      fun (n:int) ->\n\
      \          if (n = 0) then true else odd (n - 1)\n\
      \  and\n\
      \  odd: int -> bool =\n\
      \      fun (n:int) -> if (n=0) then false else even (n-1)\n\
      \ in even (8);;\n\
      \      "
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TBool,
                  Ast.Value.Constant (Ast.Constant.Boolean true),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_mutual_recursive_function_defn_then_exec _ =
    let program =
      "let rec even: int -> bool = \n\
      \      fun (n:int) ->\n\
      \          if (n = 0) then true else odd (n - 1)\n\
      \  and\n\
      \  odd: int -> bool =\n\
      \      fun (n:int) -> if (n=0) then false else even (n-1);;\n\
      \ even (8);;\n\
      \      "
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TBool,
                  Ast.Value.Constant (Ast.Constant.Boolean true),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_mutual_recursion_defn _ =
    let program =
      "let rec even: int -> bool = \n\
      \      fun (n:int) ->\n\
      \          if (n = 0) then true else odd (n - 1)\n\
      \  and\n\
      \  odd: int -> bool =\n\
      \      fun (n:int) -> if (n=0) then false else even (n-1);;\n\
      \  "
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.MutRecDefn
                [
                  ( ( Ast.ObjIdentifier.of_string "even",
                      Ast.Typ.TFun (Ast.Typ.TInt, Ast.Typ.TBool) ),
                    Ast.Value.Lambda
                      ( (Ast.ObjIdentifier.of_string "n", Ast.Typ.TInt),
                        Ast.Expr.IfThenElse
                          ( Ast.Expr.BinaryOp
                              ( Ast.BinaryOperator.EQ,
                                Ast.Expr.Identifier
                                  (Ast.ObjIdentifier.of_string_and_index "n"
                                     (create_debruijn_exn 0)),
                                Ast.Expr.Constant (Ast.Constant.Integer 0) ),
                            Ast.Expr.Constant (Ast.Constant.Boolean true),
                            Ast.Expr.Application
                              ( Ast.Expr.Identifier
                                  (Ast.ObjIdentifier.of_string_and_index "odd"
                                     (create_debruijn_exn 1)),
                                Ast.Expr.BinaryOp
                                  ( Ast.BinaryOperator.SUB,
                                    Ast.Expr.Identifier
                                      (Ast.ObjIdentifier.of_string_and_index "n"
                                         (create_debruijn_exn 0)),
                                    Ast.Expr.Constant (Ast.Constant.Integer 1)
                                  ) ) ) ),
                    None,
                    None,
                    None );
                  ( ( Ast.ObjIdentifier.of_string "odd",
                      Ast.Typ.TFun (Ast.Typ.TInt, Ast.Typ.TBool) ),
                    Ast.Value.Lambda
                      ( (Ast.ObjIdentifier.of_string "n", Ast.Typ.TInt),
                        Ast.Expr.IfThenElse
                          ( Ast.Expr.BinaryOp
                              ( Ast.BinaryOperator.EQ,
                                Ast.Expr.Identifier
                                  (Ast.ObjIdentifier.of_string_and_index "n"
                                     (create_debruijn_exn 0)),
                                Ast.Expr.Constant (Ast.Constant.Integer 0) ),
                            Ast.Expr.Constant (Ast.Constant.Boolean false),
                            Ast.Expr.Application
                              ( Ast.Expr.Identifier
                                  (Ast.ObjIdentifier.of_string_and_index "even"
                                     (create_debruijn_exn 1)),
                                Ast.Expr.BinaryOp
                                  ( Ast.BinaryOperator.SUB,
                                    Ast.Expr.Identifier
                                      (Ast.ObjIdentifier.of_string_and_index "n"
                                         (create_debruijn_exn 0)),
                                    Ast.Expr.Constant (Ast.Constant.Integer 1)
                                  ) ) ) ),
                    None,
                    None,
                    None );
                ]))
          (List.last results)
  in
  let test_datatype_mutually_recursive _ =
    let program =
      "datatype sometype = A of int | B of sometype | C of (othertype)\n\
      \      and othertype = D | E of (sometype);;      \n\
      \      E (C (D));;\n\
      \      "
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "othertype"),
                  Ast.Value.Constr
                    ( Ast.Constructor.of_string "E",
                      Some
                        (Ast.Value.Constr
                           ( Ast.Constructor.of_string "C",
                             Some
                               (Ast.Value.Constr
                                  (Ast.Constructor.of_string "D", None)) )) ),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_string _ =
    let program = "let x:string = '1'++\"123\" ^ \"5678\";;" in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.Defn
                ( (Ast.ObjIdentifier.of_string "x", Ast.Typ.TString),
                  Ast.Value.Constant (Ast.Constant.String "11235678"),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_string_match _ =
    let program = "match \"x\" with\n| a ++ as -> a\n| \"\" -> 'd';;\n" in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TChar,
                  Ast.Value.Constant (Ast.Constant.Character 'x'),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_imperative_seq_ref_assign_deref _ =
    let program = "let b: int ref = ref 123;; b:= 1; !b;;" in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TInt,
                  Ast.Value.Constant (Ast.Constant.Integer 1),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_imperative_while_fib _ =
    let program =
      "let fib: int -> int = fun (n:int) ->\n\
      \      if n <= 1 then n else\n\
      \      let fib_n_2: int ref = ref 0 in\n\
      \      let fib_n_1: int ref = ref 1 in\n\
      \      let counter: int ref = ref 2 in\n\
      \      let tmp: int ref = ref 0 in \n\
      \      while (!counter <= n) do\n\
      \          tmp := !fib_n_1;\n\
      \          fib_n_1 := !tmp + !fib_n_2;\n\
      \          fib_n_2 := !tmp;\n\
      \          counter := !counter + 1\n\
      \      done;\n\
      \      !fib_n_1;;\n\
      \  \n\
      \    (fib 0, fib 1, fib 5, fib 10);; \n\
      \    "
    in
    print_endline "Start";
    let res_opt = exec_program interpreter program in
    print_endline "end";
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TProd
                    [ Ast.Typ.TInt; Ast.Typ.TInt; Ast.Typ.TInt; Ast.Typ.TInt ],
                  Ast.Value.Prod
                    [
                      Ast.Value.Constant (Ast.Constant.Integer 0);
                      Ast.Value.Constant (Ast.Constant.Integer 1);
                      Ast.Value.Constant (Ast.Constant.Integer 5);
                      Ast.Value.Constant (Ast.Constant.Integer 55);
                    ],
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_array_len _ =
    (* Coarse test *)
    let program = "let a: int array = [|1, 2, 3|];; len (a);;" in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TInt,
                  Ast.Value.Constant (Ast.Constant.Integer 3),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_array_get_set _ =
    (* Coarse test *)
    let program =
      "let a: int array = [|1, 2, 3|];;a.(1);; a.(1) <- 421 * 134 + 2;; a.(1);;"
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TInt,
                  Ast.Value.Constant (Ast.Constant.Integer 2),
                  None,
                  None,
                  None )))
          (List.nth results 1);
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TInt,
                  Ast.Value.Constant (Ast.Constant.Integer 56416),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_array_lift _ =
    let program =
      "let a: int array = [|1, 2, 3|];;\n      lift[int array] a;;"
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TBox ([], Ast.Typ.TArray Ast.Typ.TInt),
                  Ast.Value.Box
                    ( [],
                      Ast.Expr.Array
                        [
                          Ast.Expr.Constant (Ast.Constant.Integer 1);
                          Ast.Expr.Constant (Ast.Constant.Integer 2);
                          Ast.Expr.Constant (Ast.Constant.Integer 3);
                        ] ),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let interpreter_suite =
    "interpreter_main_suite"
    >::: [
           "test_interp_int" >:: test_interp_int;
           "test_interp_var_def" >:: test_interp_var_def;
           "test_interp_rec_var_def" >:: test_interp_rec_var_def;
           "test_interp_expr" >:: test_interp_expr;
           "test_staging_pow" >:: test_staging_pow;
           "test_datatype_matching" >:: test_datatype_matching;
           "test_matching_n_ary_product" >:: test_matching_n_ary_product;
           "test_matching_inl_inr" >:: test_matching_inl_inr;
           "test_match_wildcard" >:: test_match_wildcard;
           "test_match_identifier" >:: test_match_identifier;
           "test_lift_primitive" >:: test_lift_primitive;
           "test_lift_non_primitive" >:: test_lift_non_primitive;
           "test_mutual_recursive_expr" >:: test_mutual_recursive_expr;
           "test_mutual_recursive_function_defn_then_exec"
           >:: test_mutual_recursive_function_defn_then_exec;
           "test_mutual_recursion_defn" >:: test_mutual_recursion_defn;
           "test_datatype_mutually_recursive"
           >:: test_datatype_mutually_recursive;
           "test_string" >:: test_string;
           "test_string_match" >:: test_string_match;
           "test_imperative_seq_ref_assign_deref"
           >:: test_imperative_seq_ref_assign_deref;
           "test_imperative_while_fib" >:: test_imperative_while_fib;
           "test_array_len" >:: test_array_len;
           "test_array_get_set" >:: test_array_get_set;
           "test_array_lift" >:: test_array_lift;
         ]
  in
  let test_intlist_map _ =
    let program =
      "datatype intlist = Nil | Cons of (int * intlist);;\n\n\
      \      let rec map_int_int:(intlist -> (int -> int) -> intlist) = \n\
      \          fun (xs: intlist) -> fun (f:(int -> int)) -> \n\
      \          match xs with\n\
      \          | Nil -> Nil\n\
      \          | Cons (x, xs) -> Cons (f x, map_int_int xs f);;\n\
      \      \n\
      \      map_int_int (Cons (1, Cons (2, Cons (3, Nil)))) (fun (x:int) -> x \
       * 2);;\n\
      \      \n\
      \      (*Unfortunately lifting is necessary*)\n\
      \      (*\n\
      \          let rec lift_int: int -> []int = fun (n:int) -> \n\
      \              if n = 0 then box (|- 0) else let box u = lift_int (n-1) \
       in box (|- (u with ()) + 1);;\n\
      \      *)\n\
      \      (* Powerful new lifting primitive! *)\n\
      \      let lift_int: int -> []int = fun (n:int) -> lift[int] n;;\n\
      \      \n\
      \      let rec map_int_int_b:(intlist -> [f:(int -> int)]intlist) = \n\
      \          fun (xs:intlist) -> \n\
      \          match xs with\n\
      \          | Nil -> box (f: (int -> int) |- Nil)\n\
      \          | Cons (x, xs) -> \n\
      \              let box v = map_int_int_b xs in\n\
      \              let box lifted_x = lift_int x in\n\
      \              box (f: (int -> int) |- Cons (f (lifted_x with ()), v \
       with (f)));;\n\
      \      \n\
      \      let res:([f:(int -> int)]intlist) = map_int_int_b (Cons (1, Cons \
       (2, Cons (3, Nil))));;\n\
      \      \n\
      \      let box u = res in u with (fun (x:int) -> x * 2);;\n\
      \      "
    in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "intlist"),
                  Ast.Value.Constr
                    ( Ast.Constructor.of_string "Cons",
                      Some
                        (Ast.Value.Prod
                           [
                             Ast.Value.Constant (Ast.Constant.Integer 2);
                             Ast.Value.Constr
                               ( Ast.Constructor.of_string "Cons",
                                 Some
                                   (Ast.Value.Prod
                                      [
                                        Ast.Value.Constant
                                          (Ast.Constant.Integer 4);
                                        Ast.Value.Constr
                                          ( Ast.Constructor.of_string "Cons",
                                            Some
                                              (Ast.Value.Prod
                                                 [
                                                   Ast.Value.Constant
                                                     (Ast.Constant.Integer 6);
                                                   Ast.Value.Constr
                                                     ( Ast.Constructor.of_string
                                                         "Nil",
                                                       None );
                                                 ]) );
                                      ]) );
                           ]) ),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_arr_equality _ =
    let program = "let a: int array = [|1, 2, 3|];;\n a = [|1, 2, 3|];;" in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TBool,
                  Ast.Value.Constant (Ast.Constant.Boolean false),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let test_ref_equality _ =
    let program = "let a: int ref = ref 2;;\n a = ref 2;;" in
    let res_opt = exec_program interpreter program in
    match res_opt with
    | None -> assert_string "Program Execution Failed"
    | Some results ->
        assert_equal
          (Some
             (Interpreter_common.TopLevelEvaluationResult.ExprValue
                ( Ast.Typ.TBool,
                  Ast.Value.Constant (Ast.Constant.Boolean false),
                  None,
                  None,
                  None )))
          (List.last results)
  in
  let interpreter_regression_suite =
    "interpreter_regression_suite"
    >::: [
           "test_intlist_map" >:: test_intlist_map;
           "test_arr_equality" >:: test_arr_equality;
           "test_ref_equality" >:: test_ref_equality;
         ]
  in
  let suite =
    Printf.sprintf "interpreter_suite(%s)" (Interpreter.show interpreter)
    >::: [ interpreter_suite; interpreter_regression_suite ]
  in
  suite

let suite =
  "interpreter_suite"
  >::: [
         generate_tests_for_interpreter
           (Interpreter.SingleStep { show_step_count = false; verbose = false });
         generate_tests_for_interpreter
           (Interpreter.MultiStep { show_time = false });
       ]
