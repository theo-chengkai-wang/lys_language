open OUnit2
open Lys_parsing
open Lys_typing
open Lys_ast
open Lys_interpreter
open Core

let exec_program str =
  str |> Lexing.from_string |> Lex_and_parse.parse_program
  |> Ast.Program.of_past |> Typecore.type_check_program |> ok_exn
  |> Ast.TypedProgram.populate_index |> ok_exn
  |> Interpreter.evaluate_program |> Or_error.ok

let print_res res =
  List.iter res ~f:(fun res ->
      print_endline (Interpreter.TopLevelEvaluationResult.show res);
      print_endline "")

let create_debruijn_exn n = n |> Ast.DeBruijnIndex.create |> ok_exn

let test_interp_int _ =
  let res_opt = exec_program "1;;" in
  assert_equal
    (Some
       [
         Interpreter.TopLevelEvaluationResult.ExprValue
           (Ast.Typ.TInt, Ast.Value.Constant (Ast.Constant.Integer 1));
       ])
    res_opt

let test_interp_var_def _ =
  let program = "let x:int = 0;; x;;" in
  let res_opt = exec_program program in
  (* print_res (Option.value_exn res_opt); *)
  assert_equal
    (Some
       [
         Interpreter.TopLevelEvaluationResult.Defn
           ( ( Ast.ObjIdentifier.of_string_and_index "x"
                 Ast.DeBruijnIndex.none,
               Ast.Typ.TInt ),
             Ast.Value.Constant (Ast.Constant.Integer 0) );
         Interpreter.TopLevelEvaluationResult.ExprValue
           (Ast.Typ.TInt, Ast.Value.Constant (Ast.Constant.Integer 0));
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
         Interpreter.TopLevelEvaluationResult.RecDefn
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
                                      (create_debruijn_exn 0)) ) ) ) ) ) );
         Interpreter.TopLevelEvaluationResult.ExprValue
           (Ast.Typ.TInt, Ast.Value.Constant (Ast.Constant.Integer 9));
       ])
    res_opt

let test_interp_expr _ =
  let program = "5+5;;" in
  let res_opt = exec_program program in
  assert_equal
    (Some
       [
         Interpreter.TopLevelEvaluationResult.ExprValue
           (Ast.Typ.TInt, Ast.Value.Constant (Ast.Constant.Integer 10));
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
         Interpreter.TopLevelEvaluationResult.RecDefn
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
                                     ] ) ) ) ) ) ) );
         Interpreter.TopLevelEvaluationResult.ExprValue
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
                         Ast.Expr.Constant (Ast.Constant.Integer 1) ) ) ) );
         Interpreter.TopLevelEvaluationResult.ExprValue
           (Ast.Typ.TInt, Ast.Value.Constant (Ast.Constant.Integer 9));
       ])
    res_opt

let suite =
  "interpreter_suite"
  >::: [
         "test_interp_int" >:: test_interp_int;
         "test_interp_var_def" >:: test_interp_var_def;
         "test_interp_rec_var_def" >:: test_interp_rec_var_def;
         "test_interp_expr" >:: test_interp_expr;
         "test_staging_pow" >:: test_staging_pow;
       ]
