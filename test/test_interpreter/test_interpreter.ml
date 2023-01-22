open OUnit2
open Lys_parsing
open Lys_typing
open Lys_ast
open Lys_interpreter
open Core

let exec_program str =
  str |> Lexing.from_string |> Lex_and_parse.parse_program
  |> Ast.Program.of_past |> Typecore.type_check_program |> ok_exn
  |> Ast.TypedProgram.populate_index |> ok_exn |> Interpreter.evaluate_program
  |> Or_error.ok

let print_res res =
  List.iter res ~f:(fun res ->
      print_endline (Interpreter_common.TopLevelEvaluationResult.show res);
      print_endline "")

let create_debruijn_exn n = n |> Ast.DeBruijnIndex.create |> ok_exn

let test_interp_int _ =
  let res_opt = exec_program "1;;" in
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

let test_interp_var_def _ =
  let program = "let x:int = 0;; x;;" in
  let res_opt = exec_program program in
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

let test_interp_rec_var_def _ =
  let program =
    "let rec (pow: int -> int -> int) = fun (n:int) -> if n = 0 then (fun \
     (x:int) -> 1) else (fun (x:int) -> x * (pow (n-1) x));; pow 2 3;;"
  in
  let res_opt = exec_program program in
  (* print_res (Option.value_exn res_opt); *)
  assert_equal
    (Some
       [
         Interpreter_common.TopLevelEvaluationResult.RecDefn
           ( ( Ast.ObjIdentifier.of_string_and_index "pow" Ast.DeBruijnIndex.none,
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

let test_interp_expr _ =
  let program = "5+5;;" in
  let res_opt = exec_program program in
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

let test_staging_pow _ =
  let program =
    "let rec (pow: int -> [b:int]int) = fun (n:int) -> if n = 0 then box \n\
    \      (b:int |- 1) else let box u = pow (n-1) in box (b:int |- b * (u \
     with (b)));;\n\
    \     pow 2;;\n\
    \     let box u = pow 2 in u with (3);;"
  in
  let res_opt = exec_program program in
  (* print_res (Option.value_exn res_opt); *)
  assert_equal
    (Some
       [
         Interpreter_common.TopLevelEvaluationResult.RecDefn
           ( ( Ast.ObjIdentifier.of_string_and_index "pow" Ast.DeBruijnIndex.none,
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
                                 Ast.Expr.Constant (Ast.Constant.Integer 1) ) ),
                         Ast.Expr.Box
                           ( [ (Ast.ObjIdentifier.of_string "b", Ast.Typ.TInt) ],
                             Ast.Expr.BinaryOp
                               ( Ast.BinaryOperator.MUL,
                                 Ast.Expr.Identifier
                                   (Ast.ObjIdentifier.of_string_and_index "b"
                                      (create_debruijn_exn 0)),
                                 Ast.Expr.Closure
                                   ( Ast.MetaIdentifier.of_string_and_index "u"
                                       (create_debruijn_exn 0),
                                     [
                                       Ast.Expr.Identifier
                                         (Ast.ObjIdentifier.of_string_and_index
                                            "b" (create_debruijn_exn 0));
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
  let res_opt = exec_program program in
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

let test_matching_n_ary_product _ =
  let program =
    "let x4: (int * unit * (unit * int) * int) = (1, (), ((), 2), 3);;\n\n\
    \    match x4 with\n\
    \    | (a, b, c, d) -> c;;"
  in
  let res_opt = exec_program program in
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

let test_matching_inl_inr _ =
  let program =
    "match (L[int, unit] 10) with\n\
     | L x -> x\n\
     | R y -> 0;;\n\n\
     match (R[int, unit] ()) with\n\
     | L x -> x\n\
     | R y -> 0;;"
  in
  let res_opt = exec_program program in
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
  let res_opt = exec_program program in
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
  let res_opt = exec_program program in
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

let test_lift_primitive _ =
  let program = "lift[int] 1;;" in
  let res_opt = exec_program program in
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

let test_lift_non_primitive _ =
  let program =
    "datatype tree = Lf | Br of (int * tree * tree);;\n\
    \  let t:tree = Br (1, Lf, Br (2, Lf, Lf));;\n\
    \  lift[tree] t;;"
  in
  let res_opt = exec_program program in
  match res_opt with
  | None -> assert_string "Program Execution Failed"
  | Some results ->
      assert_equal
        (Some
           (Interpreter_common.TopLevelEvaluationResult.ExprValue
              ( Ast.Typ.TBox
                  ([], Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "tree")),
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
  let res_opt = exec_program program in
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
  let res_opt = exec_program program in
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
  let res_opt = exec_program program in
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
                                  Ast.Expr.Constant (Ast.Constant.Integer 1) )
                            ) ) ),
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
                                  Ast.Expr.Constant (Ast.Constant.Integer 1) )
                            ) ) ),
                  None,
                  None,
                  None );
              ]))
        (List.last results)

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
       ]

let test_intlist_map _ =
  let program =
    "let rec (pow: int -> int -> int) = fun (n:int) -> if n = 0 then (fun \n\
    \      (x:int) -> 1) else (fun (x:int) -> x * (pow (n-1) x));;\n\
    \   \n\
    \   pow 2 3;; (*gives 9*)\n\
    \   \n\
    \   \n\
    \   let rec (pow2: int -> [b:int]int) = fun (n:int) -> \n\
    \   if n = 0 then box (b:int |- 1)\n\
    \   else \n\
    \       let box u = pow2 (n-1) in box (b:int |- b * (u with (b)));;\n\
    \   \n\
    \   pow2 2;;\n\
    \   \n\
    \   let box u = pow2 2 in u with (3);; (*gives 9*)"
  in
  let res_opt = exec_program program in
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

let interpreter_regression_suite = "interpreter_regression_suite" >::: []
let suite = "interpreter_suite" >::: [ interpreter_suite ]
