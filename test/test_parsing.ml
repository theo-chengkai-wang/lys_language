open OUnit2
open Lys_ast
open Lys_parsing.Lex_and_parse

let test_int _ =
  assert_equal (Some (Ast.Expr.Constant (Ast.Constant.Integer 2)))
    (parse_expression (Lexing.from_string "2;;"))

let test_bool_true _ =
  assert_equal (Some (Ast.Expr.Constant (Ast.Constant.Boolean true)))
    (parse_expression (Lexing.from_string "true;;"))

let test_bool_false _ =
  assert_equal (Some (Ast.Expr.Constant (Ast.Constant.Boolean false)))
    (parse_expression (Lexing.from_string "false;;"))

let test_unit _ =
  assert_equal (Some (Ast.Expr.Constant Ast.Constant.Unit))
    (parse_expression (Lexing.from_string "();;"))

let test_prod _ =
  assert_equal
    (Some (Ast.Expr.Prod (Ast.Expr.Identifier "x", Ast.Expr.Identifier "y")))
    (parse_expression (Lexing.from_string "(x, y);;"))

let test_fst _ =
  assert_equal
    (Some
       (Ast.Expr.Fst
          (Ast.Expr.Prod (Ast.Expr.Identifier "x", Ast.Expr.Identifier "y"))))
    (parse_expression (Lexing.from_string "fst (x, y);;"))

let test_snd _ =
  assert_equal
    (Some
       (Ast.Expr.Snd
          (Ast.Expr.Prod (Ast.Expr.Identifier "x", Ast.Expr.Identifier "y"))))
    (parse_expression (Lexing.from_string "snd (x, y);;"))

let test_fun _ =
  assert_equal
    (Some (Ast.Expr.Lambda (("x", Ast.Typ.TBool), Ast.Expr.Identifier "e")))
    (parse_expression (Lexing.from_string "fun (x: bool) -> e;;"))

let test_app _ =
  assert_equal
    (Some
       (Ast.Expr.Application
          ( Ast.Expr.Application
              ( Ast.Expr.Application
                  ( Ast.Expr.Application
                      ( Ast.Expr.Application
                          ( Ast.Expr.Lambda
                              (("x", Ast.Typ.TInt), Ast.Expr.Identifier "e"),
                            Ast.Expr.Identifier "f" ),
                        Ast.Expr.Identifier "g" ),
                    Ast.Expr.Identifier "h" ),
                Ast.Expr.Prod
                  ( Ast.Expr.Constant (Ast.Constant.Integer 1),
                    Ast.Expr.Constant (Ast.Constant.Integer 2) ) ),
            Ast.Expr.Left
              ( Ast.Typ.TInt,
                Ast.Typ.TInt,
                Ast.Expr.Constant (Ast.Constant.Integer 1) ) )))
    (parse_expression
       (Lexing.from_string "(fun (x:int) -> e) f g h (1, 2) (L[int, int] 1);;"))

let test_box _ =
  assert_equal
    (Some
       (Ast.Expr.Box
          ( [ ("x", Ast.Typ.TInt); ("y", Ast.Typ.TBool) ],
            Ast.Expr.Identifier "a" )))
    (parse_expression (Lexing.from_string "box (x: int, y: bool |- a);;"))

let test_unbox _ =
  assert_equal
    (Some
       (Ast.Expr.LetBox
          ( "u",
            Ast.Expr.Box
              ( [ ("x", Ast.Typ.TInt); ("y", Ast.Typ.TBool) ],
                Ast.Expr.Identifier "a" ),
            Ast.Expr.Identifier "e" )))
    (parse_expression
       (Lexing.from_string "let box u = box (x: int, y: bool |- a) in e;;"))

let test_with _ =
  assert_equal
    (Some
       (Ast.Expr.Closure
          ( "u",
            [
              Ast.Expr.Constant (Ast.Constant.Integer 1);
              Ast.Expr.Constant (Ast.Constant.Integer 2);
            ] )))
    (parse_expression (Lexing.from_string "u with (1, 2);;"))

let test_match _ =
  assert_equal
    (Some
       (Ast.Expr.Match
          ( Ast.Expr.Identifier "x",
            ("y", Ast.Typ.TInt),
            Ast.Expr.Identifier "a",
            ("z", Ast.Typ.TInt),
            Ast.Expr.Identifier "b" )))
    (parse_expression
       (Lexing.from_string "match x with L (y:int) -> a | R (z:int) -> b;;"))

let test_inl _ =
  assert_equal
    (Some
       (Ast.Expr.Left
          ( Ast.Typ.TInt,
            Ast.Typ.TBool,
            Ast.Expr.Constant (Ast.Constant.Integer 1) )))
    (parse_expression (Lexing.from_string "L[int, bool] 1;;"))

let test_inr _ =
  assert_equal
    (Some
       (Ast.Expr.Right
          ( Ast.Typ.TInt,
            Ast.Typ.TBool,
            Ast.Expr.Constant (Ast.Constant.Boolean true) )))
    (parse_expression (Lexing.from_string "R[int, bool] true;;"))

let test_reg_parse_unit_and_not_unit _ =
  assert_equal
    (Some
       (Ast.Expr.LetBinding
          ( ("x", Ast.Typ.TBox ([], Ast.Typ.TIdentifier "A")),
            Ast.Expr.Box ([], Ast.Expr.Identifier "A"),
            Ast.Expr.LetBox
              ("u", Ast.Expr.Identifier "x", Ast.Expr.Closure ("u", [])) )))
    (parse_expression
       (Lexing.from_string
          "let x: []A = box (|- A) in\nlet box u = x in\n    u with ();;\n"))

let test_let _ =
  assert_equal
    (Some
       (Ast.Expr.LetBinding
          ( ("x", Ast.Typ.TIdentifier "A"),
            Ast.Expr.Identifier "y",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "let x: A = y in b;;"))

let test_let_rec _ =
  assert_equal
    (Some
       (Ast.Expr.LetRec
          ( ( "x",
              Ast.Typ.TFun (Ast.Typ.TIdentifier "A", Ast.Typ.TIdentifier "B") ),
            Ast.Expr.Identifier "y",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "let rec x: A -> B = y in b;;"))

let test_if_else _ =
  assert_equal
    (Some
       (Ast.Expr.IfThenElse
          ( Ast.Expr.Identifier "b",
            Ast.Expr.Identifier "e1",
            Ast.Expr.Identifier "e2" )))
    (parse_expression (Lexing.from_string "if b then e1 else e2;;"))

let test_binop_plus _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.ADD,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a + b;;"))

let test_binop_sub _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.SUB,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a - b;;"))

let test_binop_mul _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.MUL,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a * b;;"))

let test_binop_div _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.DIV,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a / b;;"))

let test_binop_mod _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.MOD,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a % b;;"))

let test_binop_eq _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.EQ,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a = b;;"))

let test_binop_neq _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.NEQ,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a != b;;"))

let test_binop_lte _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.LTE,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a <= b;;"))

let test_binop_gte _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.GTE,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a >= b;;"))

let test_binop_lt _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.LT,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a < b;;"))

let test_binop_gt _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.GT,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a > b;;"))

let test_binop_and _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.AND,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a and b;;"))

let test_binop_or _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.OR,
            Ast.Expr.Identifier "a",
            Ast.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a or b;;"))

let test_unop_not _ =
  assert_equal
    (Some (Ast.Expr.UnaryOp (Ast.UnaryOperator.NOT, Ast.Expr.Identifier "a")))
    (parse_expression (Lexing.from_string "not a;;"))

let test_binop_neg _ =
  assert_equal
    (Some (Ast.Expr.UnaryOp (Ast.UnaryOperator.NEG, Ast.Expr.Identifier "a")))
    (parse_expression (Lexing.from_string "-a;;"))

let test_precedence_arith_bool _ =
  assert_equal
    (Some
       (Ast.Expr.BinaryOp
          ( Ast.BinaryOperator.OR,
            Ast.Expr.BinaryOp
              ( Ast.BinaryOperator.OR,
                Ast.Expr.BinaryOp
                  ( Ast.BinaryOperator.AND,
                    Ast.Expr.BinaryOp
                      ( Ast.BinaryOperator.EQ,
                        Ast.Expr.BinaryOp
                          ( Ast.BinaryOperator.ADD,
                            Ast.Expr.Identifier "a",
                            Ast.Expr.Identifier "b" ),
                        Ast.Expr.UnaryOp
                          (Ast.UnaryOperator.NEG, Ast.Expr.Identifier "c") ),
                    Ast.Expr.UnaryOp
                      (Ast.UnaryOperator.NOT, Ast.Expr.Identifier "d") ),
                Ast.Expr.BinaryOp
                  ( Ast.BinaryOperator.LTE,
                    Ast.Expr.Identifier "e",
                    Ast.Expr.Constant (Ast.Constant.Integer 2) ) ),
            Ast.Expr.BinaryOp
              ( Ast.BinaryOperator.GT,
                Ast.Expr.Identifier "f",
                Ast.Expr.BinaryOp
                  ( Ast.BinaryOperator.DIV,
                    Ast.Expr.Constant (Ast.Constant.Integer 10),
                    Ast.Expr.UnaryOp
                      (Ast.UnaryOperator.NEG, Ast.Expr.Identifier "x") ) ) )))
    (parse_expression
       (Lexing.from_string "a+b = -c and not d or e <= 2 or f > 10/-x;;"))

let test_comment _ =
  assert_equal (Some (Ast.Expr.Identifier "a"))
    (parse_expression
       (Lexing.from_string
          "(*something something*)a (*something something \n something8*);;"))

(*
   TODO: Migrate to expect test
*)

(*
   TODO: Tests for precedence, associativity
*)

let test_defn _ =
  assert_equal
    [
      Ast.TopLevelDefn.Definition
        (("x", Ast.Typ.TInt), Ast.Expr.Constant (Ast.Constant.Integer 2));
    ]
    (parse_program (Lexing.from_string "let (x:int) = 2;;"))

let test_rec_defn _ =
  assert_equal
    [
      Ast.TopLevelDefn.RecursiveDefinition
        ( ( "pow",
            Ast.Typ.TFun
              (Ast.Typ.TFun (Ast.Typ.TInt, Ast.Typ.TInt), Ast.Typ.TInt) ),
          Ast.Expr.Lambda
            ( ("p", Ast.Typ.TInt),
              Ast.Expr.Lambda
                ( ("x", Ast.Typ.TInt),
                  Ast.Expr.IfThenElse
                    ( Ast.Expr.BinaryOp
                        ( Ast.BinaryOperator.EQ,
                          Ast.Expr.Identifier "p",
                          Ast.Expr.Constant (Ast.Constant.Integer 0) ),
                      Ast.Expr.Constant (Ast.Constant.Integer 1),
                      Ast.Expr.BinaryOp
                        ( Ast.BinaryOperator.MUL,
                          Ast.Expr.Identifier "x",
                          Ast.Expr.Application
                            ( Ast.Expr.Identifier "pow",
                              Ast.Expr.BinaryOp
                                ( Ast.BinaryOperator.SUB,
                                  Ast.Expr.Identifier "p",
                                  Ast.Expr.Constant (Ast.Constant.Integer 1) )
                            ) ) ) ) ) );
    ]
    (parse_program
       (Lexing.from_string
          "let rec (pow: int -> int -> int) = fun (p: int) -> fun (x: int) -> \
           if p = 0 then 1 else x * (pow (p - 1));;"))

let test_program_sep _ =
  assert_equal
    [
      Ast.TopLevelDefn.Definition
        (("x", Ast.Typ.TInt), Ast.Expr.Constant (Ast.Constant.Integer 2));
      Ast.TopLevelDefn.RecursiveDefinition
        ( ( "pow",
            Ast.Typ.TFun
              (Ast.Typ.TFun (Ast.Typ.TInt, Ast.Typ.TInt), Ast.Typ.TInt) ),
          Ast.Expr.Lambda
            ( ("p", Ast.Typ.TInt),
              Ast.Expr.Lambda
                ( ("x", Ast.Typ.TInt),
                  Ast.Expr.IfThenElse
                    ( Ast.Expr.BinaryOp
                        ( Ast.BinaryOperator.EQ,
                          Ast.Expr.Identifier "p",
                          Ast.Expr.Constant (Ast.Constant.Integer 0) ),
                      Ast.Expr.Constant (Ast.Constant.Integer 1),
                      Ast.Expr.BinaryOp
                        ( Ast.BinaryOperator.MUL,
                          Ast.Expr.Identifier "x",
                          Ast.Expr.Application
                            ( Ast.Expr.Identifier "pow",
                              Ast.Expr.BinaryOp
                                ( Ast.BinaryOperator.SUB,
                                  Ast.Expr.Identifier "p",
                                  Ast.Expr.Constant (Ast.Constant.Integer 1) )
                            ) ) ) ) ) );
      Ast.TopLevelDefn.Expression
        (Ast.Expr.Application
           ( Ast.Expr.Application
               (Ast.Expr.Identifier "pow", Ast.Expr.Identifier "x"),
             Ast.Expr.Identifier "x" ));
    ]
    (parse_program
       (Lexing.from_string
          "let (x:int) = 2;;\n\
          \ let rec (pow: int -> int -> int) = fun (p: int) -> fun (x: int) -> \
           if p = 0 then 1 else x * (pow (p - 1));; \n\
           pow x x;;\n"))

let test_directive_env _ =
  assert_equal
    [ Ast.TopLevelDefn.Directive Ast.Directive.Env ]
    (parse_program (Lexing.from_string "ENV;;"))

let test_directive_quit _ =
  assert_equal
    [ Ast.TopLevelDefn.Directive Ast.Directive.Quit ]
    (parse_program (Lexing.from_string "QUIT;;"))

let test_directive_reset _ =
  assert_equal
    [ Ast.TopLevelDefn.Directive Ast.Directive.Reset ]
    (parse_program (Lexing.from_string "RESET;;"))

(* Name the test cases and group them together *)
let suite =
  "suite"
  >::: [
         "test_int" >:: test_int;
         "test_bool_true" >:: test_bool_true;
         "test_bool_false" >:: test_bool_false;
         "test_prod" >:: test_prod;
         "test_fst" >:: test_fst;
         "test_snd" >:: test_snd;
         "test_fun" >:: test_fun;
         "test_app" >:: test_app;
         "test_unit" >:: test_unit;
         "test_box" >:: test_box;
         "test_unbox" >:: test_unbox;
         "test_with" >:: test_with;
         "test_match" >:: test_match;
         "test_inl" >:: test_inl;
         "test_inr" >:: test_inr;
         "test_reg_parse_unit_and_not_unit" >:: test_reg_parse_unit_and_not_unit;
         "test_let" >:: test_let;
         "test_let_rec" >:: test_let_rec;
         "test_if_else" >:: test_if_else;
         "test_let" >:: test_let;
         "test_binop_plus" >:: test_binop_plus;
         "test_binop_sub" >:: test_binop_sub;
         "test_binop_mul" >:: test_binop_mul;
         "test_binop_div" >:: test_binop_div;
         "test_binop_mod" >:: test_binop_mod;
         "test_binop_eq" >:: test_binop_eq;
         "test_binop_neq" >:: test_binop_neq;
         "test_binop_lte" >:: test_binop_lte;
         "test_binop_gte" >:: test_binop_gte;
         "test_binop_or" >:: test_binop_or;
         "test_unop_not" >:: test_unop_not;
         "test_binop_neg" >:: test_binop_neg;
         "test_precedence_arith_bool" >:: test_precedence_arith_bool;
         "test_comment" >:: test_comment;
         "test_program_sep" >:: test_program_sep;
         "test_directive_env" >:: test_directive_env;
         "test_directive_quit" >:: test_directive_quit;
         "test_directive_reset" >:: test_directive_reset;
         "test_defn" >:: test_defn;
         "test_rec_defn" >:: test_rec_defn;
       ]

let () = run_test_tt_main suite
