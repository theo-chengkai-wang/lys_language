(*Example tests*)

open OUnit2
open Lys_ast
open Lys_parsing.Lex_and_parse

let test_int _ =
  assert_equal (Some (Ast.Constant (Ast.Integer 2)))
    (parse_expression (Lexing.from_string "2;;"))

let test_bool_true _ =
  assert_equal (Some (Ast.Constant (Ast.Boolean true)))
    (parse_expression (Lexing.from_string "true;;"))

let test_bool_false _ =
  assert_equal (Some (Ast.Constant (Ast.Boolean false)))
    (parse_expression (Lexing.from_string "false;;"))

let test_unit _ =
  assert_equal (Some (Ast.Constant Ast.Unit))
    (parse_expression (Lexing.from_string "();;"))

let test_prod _ =
  assert_equal
    (Some (Ast.Prod (Ast.Identifier "x", Ast.Identifier "y")))
    (parse_expression (Lexing.from_string "(x, y);;"))

let test_fst _ =
  assert_equal
    (Some (Ast.Fst (Ast.Prod (Ast.Identifier "x", Ast.Identifier "y"))))
    (parse_expression (Lexing.from_string "fst (x, y);;"))

let test_snd _ =
  assert_equal
    (Some (Ast.Snd (Ast.Prod (Ast.Identifier "x", Ast.Identifier "y"))))
    (parse_expression (Lexing.from_string "snd (x, y);;"))

let test_fun _ =
  assert_equal
    (Some (Ast.Lambda (("x", Ast.TBool), Identifier "e")))
    (parse_expression (Lexing.from_string "fun (x: bool) -> e;;"))

let test_app _ =
  assert_equal
    (Some
       (Ast.Application
          ( Ast.Application
              ( Ast.Application
                  ( Ast.Application
                      ( Ast.Application
                          ( Ast.Lambda (("x", Ast.TInt), Ast.Identifier "e"),
                            Ast.Identifier "f" ),
                        Ast.Identifier "g" ),
                    Ast.Identifier "h" ),
                Ast.Prod
                  (Ast.Constant (Ast.Integer 1), Ast.Constant (Ast.Integer 2))
              ),
            Ast.Left (Ast.TInt, Ast.TInt, Ast.Constant (Ast.Integer 1)) )))
    (parse_expression
       (Lexing.from_string "(fun (x:int) -> e) f g h (1, 2) (L[int, int] 1);;"))

let test_box _ =
  assert_equal
    (Some (Ast.Box ([ ("x", Ast.TInt); ("y", Ast.TBool) ], Ast.Identifier "a")))
    (parse_expression (Lexing.from_string "box (x: int, y: bool |- a);;"))

let test_unbox _ =
  assert_equal
    (Some
       (Ast.LetBox
          ( "u",
            Ast.Box ([ ("x", Ast.TInt); ("y", Ast.TBool) ], Ast.Identifier "a"),
            Ast.Identifier "e" )))
    (parse_expression
       (Lexing.from_string "let box u = box (x: int, y: bool |- a) in e;;"))

let test_with _ =
  assert_equal
    (Some
       (Ast.Closure
          ("u", [ Ast.Constant (Ast.Integer 1); Ast.Constant (Ast.Integer 2) ])))
    (parse_expression (Lexing.from_string "u with (1, 2);;"))

let test_match _ =
  assert_equal
    (Some
       (Ast.Match
          ( Ast.Identifier "x",
            ("y", Ast.TInt),
            Ast.Identifier "a",
            ("z", Ast.TInt),
            Ast.Identifier "b" )))
    (parse_expression
       (Lexing.from_string "match x with L (y:int) -> a | R (z:int) -> b;;"))

let test_inl _ =
  assert_equal
    (Some (Ast.Left (Ast.TInt, Ast.TBool, Ast.Constant (Ast.Integer 1))))
    (parse_expression (Lexing.from_string "L[int, bool] 1;;"))

let test_inr _ =
  assert_equal
    (Some (Ast.Right (Ast.TInt, Ast.TBool, Ast.Constant (Ast.Boolean true))))
    (parse_expression (Lexing.from_string "R[int, bool] true;;"))

let test_reg_parse_unit_and_not_unit _ =
  assert_equal
    (Some
       (Ast.LetBinding
          ( ("x", Ast.TBox ([], Ast.TIdentifier "A")),
            Ast.Box ([], Ast.Identifier "A"),
            Ast.LetBox ("u", Ast.Identifier "x", Ast.Closure ("u", [])) )))
    (parse_expression
       (Lexing.from_string
          "let x: []A = box (|- A) in\nlet box u = x in\n    u with ();;\n"))

let test_let _ =
  assert_equal
    (Some
       (Ast.LetBinding
          (("x", Ast.TIdentifier "A"), Ast.Identifier "y", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "let x: A = y in b;;"))

let test_let_rec _ =
  assert_equal
    (Some
       (Ast.LetRec
          ( ("x", Ast.TFun (Ast.TIdentifier "A", Ast.TIdentifier "B")),
            Ast.Identifier "y",
            Ast.Identifier "b" )))
    (parse_expression (Lexing.from_string "let rec x: A -> B = y in b;;"))

let test_if_else _ =
  assert_equal
    (Some
       (Ast.IfThenElse
          (Ast.Identifier "b", Ast.Identifier "e1", Ast.Identifier "e2")))
    (parse_expression (Lexing.from_string "if b then e1 else e2;;"))

let test_binop_plus _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.ADD, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a + b;;"))

let test_binop_sub _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.SUB, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a - b;;"))

let test_binop_mul _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.MUL, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a * b;;"))

let test_binop_div _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.DIV, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a / b;;"))

let test_binop_mod _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.MOD, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a % b;;"))

let test_binop_eq _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.EQ, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a = b;;"))

let test_binop_neq _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.NEQ, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a != b;;"))

let test_binop_lte _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.LTE, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a <= b;;"))

let test_binop_gte _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.GTE, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a >= b;;"))

let test_binop_lt _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.LTE, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a < b;;"))

let test_binop_gt _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.GTE, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a > b;;"))

let test_binop_and _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.AND, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a and b;;"))

let test_binop_or _ =
  assert_equal
    (Some (Ast.BinaryOp (Ast.OR, Ast.Identifier "a", Ast.Identifier "b")))
    (parse_expression (Lexing.from_string "a or b;;"))

let test_unop_not _ =
  assert_equal
    (Some (Ast.UnaryOp (Ast.NOT, Ast.Identifier "a")))
    (parse_expression (Lexing.from_string "not a;;"))

let test_binop_neg _ =
  assert_equal
    (Some (Ast.UnaryOp (Ast.NEG, Ast.Identifier "a")))
    (parse_expression (Lexing.from_string "-a;;"))

let test_precedence_arith_bool _ =
  assert_equal
    (Some
       (Ast.BinaryOp
          ( Ast.OR,
            Ast.BinaryOp
              ( Ast.OR,
                Ast.BinaryOp
                  ( Ast.AND,
                    Ast.BinaryOp
                      ( Ast.EQ,
                        Ast.BinaryOp
                          (Ast.ADD, Ast.Identifier "a", Ast.Identifier "b"),
                        Ast.UnaryOp (Ast.NEG, Ast.Identifier "c") ),
                    Ast.UnaryOp (Ast.NOT, Ast.Identifier "d") ),
                Ast.BinaryOp
                  (Ast.LTE, Ast.Identifier "e", Ast.Constant (Ast.Integer 2)) ),
            Ast.BinaryOp
              ( Ast.GT,
                Ast.Identifier "f",
                Ast.BinaryOp
                  ( Ast.DIV,
                    Ast.Constant (Ast.Integer 10),
                    Ast.UnaryOp (Ast.NEG, Ast.Identifier "x") ) ) )))
    (parse_expression
       (Lexing.from_string "a+b = -c and not d or e <= 2 or f > 10/-x;;"))

let test_comment _ =
  assert_equal (Some (Ast.Identifier "a"))
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
    [ Ast.Definition (("x", Ast.TInt), Ast.Constant (Ast.Integer 2)) ]
    (parse_program (Lexing.from_string "let (x:int) = 2;;"))

let test_rec_defn _ =
  assert_equal
    [
      Ast.RecursiveDefinition
        ( ("pow", Ast.TFun (Ast.TFun (Ast.TInt, Ast.TInt), Ast.TInt)),
          Ast.Lambda
            ( ("p", Ast.TInt),
              Ast.Lambda
                ( ("x", Ast.TInt),
                  Ast.IfThenElse
                    ( Ast.BinaryOp
                        ( Ast.EQ,
                          Ast.Identifier "p",
                          Ast.Constant (Ast.Integer 0) ),
                      Ast.Constant (Ast.Integer 1),
                      Ast.BinaryOp
                        ( Ast.MUL,
                          Ast.Identifier "x",
                          Ast.Application
                            ( Ast.Identifier "pow",
                              Ast.BinaryOp
                                ( Ast.SUB,
                                  Ast.Identifier "p",
                                  Ast.Constant (Ast.Integer 1) ) ) ) ) ) ) );
    ]
    (parse_program
       (Lexing.from_string
          "let rec (pow: int -> int -> int) = fun (p: int) -> fun (x: int) -> \
           if p = 0 then 1 else x * (pow (p - 1));;"))

let test_program_sep _ =
  assert_equal
    [
      Ast.Definition (("x", Ast.TInt), Ast.Constant (Ast.Integer 2));
      Ast.RecursiveDefinition
        ( ("pow", Ast.TFun (Ast.TFun (Ast.TInt, Ast.TInt), Ast.TInt)),
          Ast.Lambda
            ( ("p", Ast.TInt),
              Ast.Lambda
                ( ("x", Ast.TInt),
                  Ast.IfThenElse
                    ( Ast.BinaryOp
                        ( Ast.EQ,
                          Ast.Identifier "p",
                          Ast.Constant (Ast.Integer 0) ),
                      Ast.Constant (Ast.Integer 1),
                      Ast.BinaryOp
                        ( Ast.MUL,
                          Ast.Identifier "x",
                          Ast.Application
                            ( Ast.Identifier "pow",
                              Ast.BinaryOp
                                ( Ast.SUB,
                                  Ast.Identifier "p",
                                  Ast.Constant (Ast.Integer 1) ) ) ) ) ) ) );
      Ast.Expression
        (Ast.Application
           ( Ast.Application (Ast.Identifier "pow", Ast.Identifier "x"),
             Ast.Identifier "x" ));
    ]
    (parse_program
       (Lexing.from_string
          "let (x:int) = 2;;\n\
          \ let rec (pow: int -> int -> int) = fun (p: int) -> fun (x: int) -> \
           if p = 0 then 1 else x * (pow (p - 1));; \n\
           pow x x;;\n"))

let test_directive_env _ =
  assert_equal [ Ast.Directive Ast.Env ]
    (parse_program (Lexing.from_string "ENV;;"))

let test_directive_quit _ =
  assert_equal [ Ast.Directive Ast.Quit ]
    (parse_program (Lexing.from_string "QUIT;;"))

let test_directive_reset _ =
  assert_equal
    [ Ast.Directive Ast.Reset ]
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
