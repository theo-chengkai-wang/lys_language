open OUnit2
open Lys_ast
open Lys_parsing.Lex_and_parse

let test_int _ =
  assert_equal (Some (Past.Expr.Constant (Past.Constant.Integer 2)))
    (parse_expression (Lexing.from_string "2;;"))

let test_bool_true _ =
  assert_equal (Some (Past.Expr.Constant (Past.Constant.Boolean true)))
    (parse_expression (Lexing.from_string "true;;"))

let test_bool_false _ =
  assert_equal (Some (Past.Expr.Constant (Past.Constant.Boolean false)))
    (parse_expression (Lexing.from_string "false;;"))

let test_unit _ =
  assert_equal (Some (Past.Expr.Constant Past.Constant.Unit))
    (parse_expression (Lexing.from_string "();;"))

let test_prod _ =
  assert_equal
    (Some
       (Past.Expr.Prod [ Past.Expr.Identifier "x"; Past.Expr.Identifier "y" ]))
    (parse_expression (Lexing.from_string "(x, y);;"))

(* let test_fst _ =
     assert_equal
       (Some
          (Past.Expr.Fst
             (Past.Expr.Prod (Past.Expr.Identifier "x", Past.Expr.Identifier "y"))))
       (parse_expression (Lexing.from_string "fst (x, y);;"))

   let test_snd _ =
     assert_equal
       (Some
          (Past.Expr.Snd
             (Past.Expr.Prod (Past.Expr.Identifier "x", Past.Expr.Identifier "y"))))
       (parse_expression (Lexing.from_string "snd (x, y);;")) *)

let test_nth _ =
  assert_equal
    (Some
       (Past.Expr.Nth
          ( Past.Expr.Prod [ Past.Expr.Identifier "x"; Past.Expr.Identifier "y" ],
            0 )))
    (parse_expression (Lexing.from_string "(x, y)[0];;"))

let test_fun _ =
  assert_equal
    (Some (Past.Expr.Lambda (("x", Past.Typ.TBool), Past.Expr.Identifier "e")))
    (parse_expression (Lexing.from_string "fun (x: bool) -> e;;"))

let test_app _ =
  assert_equal
    (Some
       (Past.Expr.Application
          ( Past.Expr.Application
              ( Past.Expr.Application
                  ( Past.Expr.Application
                      ( Past.Expr.Application
                          ( Past.Expr.Lambda
                              (("x", Past.Typ.TInt), Past.Expr.Identifier "e"),
                            Past.Expr.Identifier "f" ),
                        Past.Expr.Identifier "g" ),
                    Past.Expr.Identifier "h" ),
                Past.Expr.Prod
                  ( [Past.Expr.Constant (Past.Constant.Integer 1);
                    Past.Expr.Constant (Past.Constant.Integer 2)] ) ),
            Past.Expr.Left
              ( Past.Typ.TInt,
                Past.Typ.TInt,
                Past.Expr.Constant (Past.Constant.Integer 1) ) )))
    (parse_expression
       (Lexing.from_string "(fun (x:int) -> e) f g h (1, 2) (L[int, int] 1);;"))

let test_box _ =
  assert_equal
    (Some
       (Past.Expr.Box
          ( [ ("x", Past.Typ.TInt); ("y", Past.Typ.TBool) ],
            Past.Expr.Identifier "a" )))
    (parse_expression (Lexing.from_string "box (x: int, y: bool |- a);;"))

let test_unbox _ =
  assert_equal
    (Some
       (Past.Expr.LetBox
          ( "u",
            Past.Expr.Box
              ( [ ("x", Past.Typ.TInt); ("y", Past.Typ.TBool) ],
                Past.Expr.Identifier "a" ),
            Past.Expr.Identifier "e" )))
    (parse_expression
       (Lexing.from_string "let box u = box (x: int, y: bool |- a) in e;;"))

let test_with _ =
  assert_equal
    (Some
       (Past.Expr.Closure
          ( "u",
            [
              Past.Expr.Constant (Past.Constant.Integer 1);
              Past.Expr.Constant (Past.Constant.Integer 2);
            ] )))
    (parse_expression (Lexing.from_string "u with (1, 2);;"))

let test_case _ =
  assert_equal
    (Some
       (Past.Expr.Case
          ( Past.Expr.Identifier "x",
            ("y", Past.Typ.TInt),
            Past.Expr.Identifier "a",
            ("z", Past.Typ.TInt),
            Past.Expr.Identifier "b" )))
    (parse_expression
       (Lexing.from_string "case x of L (y:int) -> a | R (z:int) -> b;;"))

let test_inl _ =
  assert_equal
    (Some
       (Past.Expr.Left
          ( Past.Typ.TInt,
            Past.Typ.TBool,
            Past.Expr.Constant (Past.Constant.Integer 1) )))
    (parse_expression (Lexing.from_string "L[int, bool] 1;;"))

let test_inr _ =
  assert_equal
    (Some
       (Past.Expr.Right
          ( Past.Typ.TInt,
            Past.Typ.TBool,
            Past.Expr.Constant (Past.Constant.Boolean true) )))
    (parse_expression (Lexing.from_string "R[int, bool] true;;"))

let test_reg_parse_unit_and_not_unit _ =
  assert_equal
    (Some
       (Past.Expr.LetBinding
          ( ("x", Past.Typ.TBox ([], Past.Typ.TIdentifier "_A")),
            Past.Expr.Box ([], Past.Expr.Identifier "_A"),
            Past.Expr.LetBox
              ("u", Past.Expr.Identifier "x", Past.Expr.Closure ("u", [])) )))
    (parse_expression
       (Lexing.from_string
          "let x: []_A = box (|- _A) in\nlet box u = x in\n    u with ();;\n"))

let test_let _ =
  assert_equal
    (Some
       (Past.Expr.LetBinding
          ( ("x", Past.Typ.TIdentifier "_A"),
            Past.Expr.Identifier "y",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "let x: _A = y in b;;"))

let test_let_rec _ =
  assert_equal
    (Some
       (Past.Expr.LetRec
          ( ( "x",
              Past.Typ.TFun
                (Past.Typ.TIdentifier "_A", Past.Typ.TIdentifier "_B") ),
            Past.Expr.Identifier "y",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "let rec x: _A -> _B = y in b;;"))

let test_if_else _ =
  assert_equal
    (Some
       (Past.Expr.IfThenElse
          ( Past.Expr.Identifier "b",
            Past.Expr.Identifier "e1",
            Past.Expr.Identifier "e2" )))
    (parse_expression (Lexing.from_string "if b then e1 else e2;;"))

let test_binop_plus _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.ADD,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a + b;;"))

let test_binop_sub _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.SUB,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a - b;;"))

let test_binop_mul _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.MUL,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a * b;;"))

let test_binop_div _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.DIV,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a / b;;"))

let test_binop_mod _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.MOD,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a % b;;"))

let test_binop_eq _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.EQ,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a = b;;"))

let test_binop_neq _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.NEQ,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a != b;;"))

let test_binop_lte _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.LTE,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a <= b;;"))

let test_binop_gte _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.GTE,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a >= b;;"))

let test_binop_lt _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.LT,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a < b;;"))

let test_binop_gt _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.GT,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a > b;;"))

let test_binop_and _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.AND,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a and b;;"))

let test_binop_or _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.OR,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a or b;;"))

let test_unop_not _ =
  assert_equal
    (Some (Past.Expr.UnaryOp (Past.UnaryOperator.NOT, Past.Expr.Identifier "a")))
    (parse_expression (Lexing.from_string "not a;;"))

let test_binop_neg _ =
  assert_equal
    (Some (Past.Expr.UnaryOp (Past.UnaryOperator.NEG, Past.Expr.Identifier "a")))
    (parse_expression (Lexing.from_string "-a;;"))

let test_precedence_arith_bool _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.OR,
            Past.Expr.BinaryOp
              ( Past.BinaryOperator.OR,
                Past.Expr.BinaryOp
                  ( Past.BinaryOperator.AND,
                    Past.Expr.BinaryOp
                      ( Past.BinaryOperator.EQ,
                        Past.Expr.BinaryOp
                          ( Past.BinaryOperator.ADD,
                            Past.Expr.Identifier "a",
                            Past.Expr.Identifier "b" ),
                        Past.Expr.UnaryOp
                          (Past.UnaryOperator.NEG, Past.Expr.Identifier "c") ),
                    Past.Expr.UnaryOp
                      (Past.UnaryOperator.NOT, Past.Expr.Identifier "d") ),
                Past.Expr.BinaryOp
                  ( Past.BinaryOperator.LTE,
                    Past.Expr.Identifier "e",
                    Past.Expr.Constant (Past.Constant.Integer 2) ) ),
            Past.Expr.BinaryOp
              ( Past.BinaryOperator.GT,
                Past.Expr.Identifier "f",
                Past.Expr.BinaryOp
                  ( Past.BinaryOperator.DIV,
                    Past.Expr.Constant (Past.Constant.Integer 10),
                    Past.Expr.UnaryOp
                      (Past.UnaryOperator.NEG, Past.Expr.Identifier "x") ) ) )))
    (parse_expression
       (Lexing.from_string "a+b = -c and not d or e <= 2 or f > 10/-x;;"))

let test_comment _ =
  assert_equal (Some (Past.Expr.Identifier "a"))
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
      Past.TopLevelDefn.Definition
        (("x", Past.Typ.TInt), Past.Expr.Constant (Past.Constant.Integer 2));
    ]
    (parse_program (Lexing.from_string "let (x:int) = 2;;"))

let test_rec_defn _ =
  assert_equal
    [
      Past.TopLevelDefn.RecursiveDefinition
        ( ( "pow",
            Past.Typ.TFun
              (Past.Typ.TInt, Past.Typ.TFun (Past.Typ.TInt, Past.Typ.TInt)) ),
          Past.Expr.Lambda
            ( ("p", Past.Typ.TInt),
              Past.Expr.Lambda
                ( ("x", Past.Typ.TInt),
                  Past.Expr.IfThenElse
                    ( Past.Expr.BinaryOp
                        ( Past.BinaryOperator.EQ,
                          Past.Expr.Identifier "p",
                          Past.Expr.Constant (Past.Constant.Integer 0) ),
                      Past.Expr.Constant (Past.Constant.Integer 1),
                      Past.Expr.BinaryOp
                        ( Past.BinaryOperator.MUL,
                          Past.Expr.Identifier "x",
                          Past.Expr.Application
                            ( Past.Expr.Application
                                ( Past.Expr.Identifier "pow",
                                  Past.Expr.BinaryOp
                                    ( Past.BinaryOperator.SUB,
                                      Past.Expr.Identifier "p",
                                      Past.Expr.Constant
                                        (Past.Constant.Integer 1) ) ),
                              Past.Expr.Identifier "x" ) ) ) ) ) );
    ]
    (parse_program
       (Lexing.from_string
          "let rec (pow: int -> int -> int) = fun (p: int) -> fun (x: int) -> \
           if p = 0 then 1 else x * (pow (p - 1) x);;"))

let test_program_sep _ =
  assert_equal
    [
      Past.TopLevelDefn.Definition
        (("x", Past.Typ.TInt), Past.Expr.Constant (Past.Constant.Integer 2));
      Past.TopLevelDefn.RecursiveDefinition
        ( ( "pow",
            Past.Typ.TFun
              (Past.Typ.TInt, Past.Typ.TFun (Past.Typ.TInt, Past.Typ.TInt)) ),
          Past.Expr.Lambda
            ( ("p", Past.Typ.TInt),
              Past.Expr.Lambda
                ( ("x", Past.Typ.TInt),
                  Past.Expr.IfThenElse
                    ( Past.Expr.BinaryOp
                        ( Past.BinaryOperator.EQ,
                          Past.Expr.Identifier "p",
                          Past.Expr.Constant (Past.Constant.Integer 0) ),
                      Past.Expr.Constant (Past.Constant.Integer 1),
                      Past.Expr.BinaryOp
                        ( Past.BinaryOperator.MUL,
                          Past.Expr.Identifier "x",
                          Past.Expr.Application
                            ( Past.Expr.Identifier "pow",
                              Past.Expr.BinaryOp
                                ( Past.BinaryOperator.SUB,
                                  Past.Expr.Identifier "p",
                                  Past.Expr.Constant (Past.Constant.Integer 1)
                                ) ) ) ) ) ) );
      Past.TopLevelDefn.Expression
        (Past.Expr.Application
           ( Past.Expr.Application
               (Past.Expr.Identifier "pow", Past.Expr.Identifier "x"),
             Past.Expr.Identifier "x" ));
    ]
    (parse_program
       (Lexing.from_string
          "let (x:int) = 2;;\n\
          \ let rec (pow: int -> int -> int) = fun (p: int) -> fun (x: int) -> \
           if p = 0 then 1 else x * (pow (p - 1));; \n\
           pow x x;;\n"))

let test_directive_env _ =
  assert_equal
    [ Past.TopLevelDefn.Directive Past.Directive.Env ]
    (parse_program (Lexing.from_string "ENV;;"))

let test_directive_quit _ =
  assert_equal
    [ Past.TopLevelDefn.Directive Past.Directive.Quit ]
    (parse_program (Lexing.from_string "QUIT;;"))

let test_directive_reset _ =
  assert_equal
    [ Past.TopLevelDefn.Directive Past.Directive.Reset ]
    (parse_program (Lexing.from_string "RESET;;"))

let test_datatype_def _ =
  assert_equal
    [
      Past.TopLevelDefn.DatatypeDecl
        ( "sometype",
          [
            ("Con1", Past.Typ.TInt);
            ("Con3", Past.Typ.TUnit);
            ( "Con4",
              Past.Typ.TProd ([Past.Typ.TInt; Past.Typ.TIdentifier "sometype"]) );
          ] );
    ]
    (parse_program
       (Lexing.from_string
          "datatype sometype = Con1 of int | Con3 of unit | Con4 of (int * \
           sometype);;"))

let test_datatype_definition _ =
  assert_equal
    [
      Past.TopLevelDefn.Definition
        ( ("x", Past.Typ.TIdentifier "sometype"),
          Past.Expr.Constr ("Con1", Past.Expr.Constant (Past.Constant.Integer 1))
        );
    ]
    (parse_program (Lexing.from_string "let x:sometype = Con1 1;;"));
  assert_equal
    [
      Past.TopLevelDefn.Definition
        ( ("y", Past.Typ.TIdentifier "sometype"),
          Past.Expr.Constr
            ( "Con4",
              Past.Expr.Prod
                ( [Past.Expr.Constant (Past.Constant.Integer 1);
                  Past.Expr.Constr
                    ("Con2", Past.Expr.Constant (Past.Constant.Integer 1))] ) )
        );
    ]
    (parse_program (Lexing.from_string "let y:sometype = Con4 (1, Con2 1);;"))

let test_match_clause _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.Match
           ( Past.Expr.Identifier "y",
             [
               ( Past.Pattern.Datatype ("Con1", [ "x" ]),
                 Past.Expr.Identifier "x" );
               ( Past.Pattern.Wildcard,
                 Past.Expr.Constant (Past.Constant.Integer 1) );
             ] ));
    ]
    (parse_program
       (Lexing.from_string
          "match y with\n         | Con1 (x) -> x\n         | _ -> 1;;\n     "))

(* Name the test cases and group them together *)
let suite =
  "parsing_suite"
  >::: [
         "test_int" >:: test_int;
         "test_bool_true" >:: test_bool_true;
         "test_bool_false" >:: test_bool_false;
         "test_prod" >:: test_prod;
         (* "test_fst" >:: test_fst;
         "test_snd" >:: test_snd; *)
         "test_nth" >:: test_nth;
         "test_fun" >:: test_fun;
         "test_app" >:: test_app;
         "test_unit" >:: test_unit;
         "test_box" >:: test_box;
         "test_unbox" >:: test_unbox;
         "test_with" >:: test_with;
         "test_case" >:: test_case;
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
         "test_datatype_def" >:: test_datatype_def;
         "test_datatype_definition" >:: test_datatype_definition;
         "test_match_clause" >:: test_match_clause;
       ]
