open OUnit2
open Lys_ast
open Lys_parsing.Lex_and_parse

let test_int _ =
  assert_equal (Some (Past.Expr.Constant (Past.Constant.Integer 2)))
    (parse_expression (Lexing.from_string "2;;"))

let test_char _ =
  assert_equal (Some (Past.Expr.Constant (Past.Constant.Character 'c')))
    (parse_expression (Lexing.from_string "'c';;"));
  assert_equal (Some (Past.Expr.Constant (Past.Constant.Character '\n')))
    (parse_expression (Lexing.from_string "'\\n';;"))

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
                  [
                    Past.Expr.Constant (Past.Constant.Integer 1);
                    Past.Expr.Constant (Past.Constant.Integer 2);
                  ] ),
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
          ( [],
            [ ("x", Past.Typ.TInt); ("y", Past.Typ.TBool) ],
            Past.Expr.Identifier "a" )))
    (parse_expression (Lexing.from_string "box (x: int, y: bool |- a);;"))

let test_unbox _ =
  assert_equal
    (Some
       (Past.Expr.LetBox
          ( "u",
            Past.Expr.Box
              ( [],
                [ ("x", Past.Typ.TInt); ("y", Past.Typ.TBool) ],
                Past.Expr.Identifier "a" ),
            Past.Expr.Identifier "e" )))
    (parse_expression
       (Lexing.from_string "let box u = box (x: int, y: bool |- a) in e;;"))

let test_with _ =
  assert_equal
    (Some
       (Past.Expr.Closure
          ( "u",
            [],
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
          ( ("x", Past.Typ.TBox ([], [], Past.Typ.TIdentifier ([], "_A"))),
            Past.Expr.Box ([], [], Past.Expr.Identifier "_A"),
            Past.Expr.LetBox
              ("u", Past.Expr.Identifier "x", Past.Expr.Closure ("u", [], []))
          )))
    (parse_expression
       (Lexing.from_string
          "let x: []_A = box (|- _A) in\nlet box u = x in\n    u with ();;\n"))

let test_let _ =
  assert_equal
    (Some
       (Past.Expr.LetBinding
          ( ("x", Past.Typ.TIdentifier ([], "_A")),
            Past.Expr.Identifier "y",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "let x: _A = y in b;;"))

let test_let_rec _ =
  assert_equal
    (Some
       (Past.Expr.LetRec
          ( ( "x",
              Past.Typ.TFun
                ( Past.Typ.TIdentifier ([], "_A"),
                  Past.Typ.TIdentifier ([], "_B") ) ),
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
    (parse_expression (Lexing.from_string "a && b;;"))

let test_binop_or _ =
  assert_equal
    (Some
       (Past.Expr.BinaryOp
          ( Past.BinaryOperator.OR,
            Past.Expr.Identifier "a",
            Past.Expr.Identifier "b" )))
    (parse_expression (Lexing.from_string "a || b;;"))

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
       (Lexing.from_string "a+b = -c && not d || e <= 2 || f > 10/-x;;"))

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
        [
          ( [],
            "sometype",
            [
              ("Con1", Some Past.Typ.TInt);
              ("Con3", Some Past.Typ.TUnit);
              ( "Con4",
                Some
                  (Past.Typ.TProd
                     [ Past.Typ.TInt; Past.Typ.TIdentifier ([], "sometype") ])
              );
            ] );
        ];
    ]
    (parse_program
       (Lexing.from_string
          "datatype sometype = Con1 of int | Con3 of unit | Con4 of (int * \
           sometype);;"))

let test_datatype_definition _ =
  assert_equal
    [
      Past.TopLevelDefn.Definition
        ( ("x", Past.Typ.TIdentifier ([], "sometype")),
          Past.Expr.Constr
            ("Con1", [], Some (Past.Expr.Constant (Past.Constant.Integer 1))) );
    ]
    (parse_program (Lexing.from_string "let x:sometype = Con1 1;;"));
  assert_equal
    [
      Past.TopLevelDefn.Definition
        ( ("y", Past.Typ.TIdentifier ([], "sometype")),
          Past.Expr.Constr
            ( "Con4",
              [],
              Some
                (Past.Expr.Prod
                   [
                     Past.Expr.Constant (Past.Constant.Integer 1);
                     Past.Expr.Constr
                       ( "Con2",
                         [],
                         Some (Past.Expr.Constant (Past.Constant.Integer 1)) );
                   ]) ) );
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

let test_lift _ =
  let program = "lift[int] 1;;" in
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.Lift
           (Past.Typ.TInt, Past.Expr.Constant (Past.Constant.Integer 1)));
    ]
    (program |> Lexing.from_string |> parse_program)

let test_mutual_recursion_defn _ =
  let program =
    "let rec even: int -> bool = \n\
    \      fun (n:int) ->\n\
    \          if (n = 0) then true else odd (n - 1)\n\
    \  and\n\
    \  odd: int -> bool =\n\
    \      fun (n:int) -> if (n=0) then false else even (n-1)\n\
    \  ;;\n\
    \  "
  in
  assert_equal
    [
      Past.TopLevelDefn.MutualRecursiveDefinition
        [
          ( ("even", Past.Typ.TFun (Past.Typ.TInt, Past.Typ.TBool)),
            Past.Expr.Lambda
              ( ("n", Past.Typ.TInt),
                Past.Expr.IfThenElse
                  ( Past.Expr.BinaryOp
                      ( Past.BinaryOperator.EQ,
                        Past.Expr.Identifier "n",
                        Past.Expr.Constant (Past.Constant.Integer 0) ),
                    Past.Expr.Constant (Past.Constant.Boolean true),
                    Past.Expr.Application
                      ( Past.Expr.Identifier "odd",
                        Past.Expr.BinaryOp
                          ( Past.BinaryOperator.SUB,
                            Past.Expr.Identifier "n",
                            Past.Expr.Constant (Past.Constant.Integer 1) ) ) )
              ) );
          ( ("odd", Past.Typ.TFun (Past.Typ.TInt, Past.Typ.TBool)),
            Past.Expr.Lambda
              ( ("n", Past.Typ.TInt),
                Past.Expr.IfThenElse
                  ( Past.Expr.BinaryOp
                      ( Past.BinaryOperator.EQ,
                        Past.Expr.Identifier "n",
                        Past.Expr.Constant (Past.Constant.Integer 0) ),
                    Past.Expr.Constant (Past.Constant.Boolean false),
                    Past.Expr.Application
                      ( Past.Expr.Identifier "even",
                        Past.Expr.BinaryOp
                          ( Past.BinaryOperator.SUB,
                            Past.Expr.Identifier "n",
                            Past.Expr.Constant (Past.Constant.Integer 1) ) ) )
              ) );
        ];
    ]
    (program |> Lexing.from_string |> parse_program)

let test_mutual_recursion_expr _ =
  let program =
    "let rec even: int -> bool = \n\
    \          fun (n:int) ->\n\
    \              if (n = 0) then true else odd (n - 1)\n\
    \      and\n\
    \      odd: int -> bool =\n\
    \          fun (n:int) -> if (n=0) then false else even (n-1)\n\
    \      in\n\
    \      even (1);;\n\
    \      "
  in
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.LetRecMutual
           ( [
               ( ("even", Past.Typ.TFun (Past.Typ.TInt, Past.Typ.TBool)),
                 Past.Expr.Lambda
                   ( ("n", Past.Typ.TInt),
                     Past.Expr.IfThenElse
                       ( Past.Expr.BinaryOp
                           ( Past.BinaryOperator.EQ,
                             Past.Expr.Identifier "n",
                             Past.Expr.Constant (Past.Constant.Integer 0) ),
                         Past.Expr.Constant (Past.Constant.Boolean true),
                         Past.Expr.Application
                           ( Past.Expr.Identifier "odd",
                             Past.Expr.BinaryOp
                               ( Past.BinaryOperator.SUB,
                                 Past.Expr.Identifier "n",
                                 Past.Expr.Constant (Past.Constant.Integer 1) )
                           ) ) ) );
               ( ("odd", Past.Typ.TFun (Past.Typ.TInt, Past.Typ.TBool)),
                 Past.Expr.Lambda
                   ( ("n", Past.Typ.TInt),
                     Past.Expr.IfThenElse
                       ( Past.Expr.BinaryOp
                           ( Past.BinaryOperator.EQ,
                             Past.Expr.Identifier "n",
                             Past.Expr.Constant (Past.Constant.Integer 0) ),
                         Past.Expr.Constant (Past.Constant.Boolean false),
                         Past.Expr.Application
                           ( Past.Expr.Identifier "even",
                             Past.Expr.BinaryOp
                               ( Past.BinaryOperator.SUB,
                                 Past.Expr.Identifier "n",
                                 Past.Expr.Constant (Past.Constant.Integer 1) )
                           ) ) ) );
             ],
             Past.Expr.Application
               ( Past.Expr.Identifier "even",
                 Past.Expr.Constant (Past.Constant.Integer 1) ) ));
    ]
    (program |> Lexing.from_string |> parse_program)

let test_mutual_recursive_datatype _ =
  assert_equal
    [
      Past.TopLevelDefn.DatatypeDecl
        [
          ( [],
            "sometype",
            [
              ("A", Some Past.Typ.TInt);
              ("B", Some (Past.Typ.TIdentifier ([], "sometype")));
              ("C", Some (Past.Typ.TIdentifier ([], "othertype")));
            ] );
          ( [],
            "othertype",
            [ ("D", None); ("E", Some (Past.Typ.TIdentifier ([], "sometype"))) ]
          );
        ];
    ]
    (parse_program
       (Lexing.from_string
          "datatype sometype = A of int | B of sometype | C of (othertype)\n\
          \          and othertype = D | E of (sometype);;"))

let test_string _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.Constant (Past.Constant.String "123"));
    ]
    (parse_program (Lexing.from_string "\"123\";;"))

let test_string_operators _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.BinaryOp
           ( Past.BinaryOperator.STRINGCONCAT,
             Past.Expr.BinaryOp
               ( Past.BinaryOperator.CHARSTRINGCONCAT,
                 Past.Expr.Constant (Past.Constant.Character 'a'),
                 Past.Expr.BinaryOp
                   ( Past.BinaryOperator.CHARSTRINGCONCAT,
                     Past.Expr.Constant (Past.Constant.Character '1'),
                     Past.Expr.Constant (Past.Constant.String "12345") ) ),
             Past.Expr.Constant (Past.Constant.String "12345\n\t") ));
    ]
    (parse_program
       (Lexing.from_string "'a'++('1'++\"12345\") ^ \"12345\n\t\";;"))

let test_string_match _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.Match
           ( Past.Expr.Identifier "x",
             [
               ( Past.Pattern.ConcatCharString ("a", "as"),
                 Past.Expr.Identifier "a" );
               ( Past.Pattern.String "",
                 Past.Expr.Constant (Past.Constant.Character 'd') );
             ] ));
    ]
    (parse_program
       (Lexing.from_string
          "match x with\n    | a ++ as -> a\n    | \"\" -> 'd';;"))

let test_deref _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.UnaryOp (Past.UnaryOperator.DEREF, Past.Expr.Identifier "b"));
    ]
    (parse_program (Lexing.from_string "!b;;"))

let test_ref_type _ =
  assert_equal
    [
      Past.TopLevelDefn.Definition
        ( ("a", Past.Typ.TRef Past.Typ.TInt),
          Past.Expr.Ref (Past.Expr.Constant (Past.Constant.Integer 0)) );
    ]
    (parse_program (Lexing.from_string "let a:int ref = ref 0;;"))

let test_ref _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.Ref (Past.Expr.Constant (Past.Constant.Integer 123)));
    ]
    (parse_program (Lexing.from_string "ref 123;;"))

let test_seq _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.BinaryOp
           ( Past.BinaryOperator.SEQ,
             Past.Expr.Identifier "a",
             Past.Expr.BinaryOp
               ( Past.BinaryOperator.SEQ,
                 Past.Expr.Identifier "b",
                 Past.Expr.Identifier "c" ) ));
    ]
    (parse_program (Lexing.from_string "a;b;c;;"))

let test_while _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.While
           ( Past.Expr.BinaryOp
               ( Past.BinaryOperator.GT,
                 Past.Expr.Identifier "d",
                 Past.Expr.Constant (Past.Constant.Integer 0) ),
             Past.Expr.BinaryOp
               ( Past.BinaryOperator.SEQ,
                 Past.Expr.Constant Past.Constant.Unit,
                 Past.Expr.Identifier "something_else" ) ));
    ]
    (parse_program
       (Lexing.from_string "while d > 0 do (); something_else done;;"))

let test_seq_precedence _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.LetBinding
           ( ("a", Past.Typ.TInt),
             Past.Expr.Constant (Past.Constant.Integer 0),
             Past.Expr.BinaryOp
               ( Past.BinaryOperator.SEQ,
                 Past.Expr.Identifier "b",
                 Past.Expr.Identifier "c" ) ));
    ]
    (parse_program (Lexing.from_string "let a: int = 0 in b; c;;"))

let test_type_app _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.Application
           ( Past.Expr.Application
               ( Past.Expr.TypeApply
                   ( Past.Expr.Application
                       ( Past.Expr.TypeApply
                           (Past.Expr.Identifier "f", Past.Typ.TInt),
                         Past.Expr.Identifier "s" ),
                     Past.Typ.TInt ),
                 Past.Expr.Identifier "d" ),
             Past.Expr.Identifier "e" ));
    ]
    (parse_program (Lexing.from_string "f [int] s [int] d e;;"))

let test_big_lambda _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.BigLambda ("a", Past.Expr.Identifier "e"));
      Past.TopLevelDefn.Expression
        (Past.Expr.BigLambda
           ( "a",
             Past.Expr.BinaryOp
               ( Past.BinaryOperator.ARRAY_INDEX,
                 Past.Expr.Identifier "b",
                 Past.Expr.Identifier "i" ) ));
      Past.TopLevelDefn.Expression
        (Past.Expr.BigLambda
           ( "a",
             Past.Expr.LetBinding
               ( ("a", Past.Typ.TVar "a"),
                 Past.Expr.Constant (Past.Constant.Integer 0),
                 Past.Expr.Identifier "b" ) ));
    ]
    (parse_program
       (Lexing.from_string "'a. e;;\n  'a. b.(i);;\n  'a. let a: 'a = 0 in b;;"))

let test_big_lambda_precedance_wrt_func _ =
  assert_equal
    [
      Past.TopLevelDefn.Definition
        ( ( "f",
            Past.Typ.TForall
              ( "b",
                Past.Typ.TForall
                  ("a", Past.Typ.TFun (Past.Typ.TVar "a", Past.Typ.TInt)) ) ),
          Past.Expr.BigLambda
            ( "b",
              Past.Expr.BigLambda
                ( "a",
                  Past.Expr.Lambda
                    ( ("x", Past.Typ.TVar "a"),
                      Past.Expr.Constant (Past.Constant.Integer 1) ) ) ) );
    ]
    (parse_program
       (Lexing.from_string
          "let f: forall 'b. forall 'a. 'a -> int = 'b.('a. fun (x: 'a) -> 1);;"))

let test_poly_box _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.Box
           ([ "a" ], [ ("x", Past.Typ.TVar "a") ], Past.Expr.Identifier "x"));
    ]
    (parse_program (Lexing.from_string "box ('a; x: 'a |- x);;"))

let test_poly_box_defn _ =
  assert_equal
    [
      Past.TopLevelDefn.Definition
        ( ( "x",
            Past.Typ.TBox
              ([ "a" ], [ ("x", Past.Typ.TVar "a") ], Past.Typ.TVar "a") ),
          Past.Expr.Box
            ([ "a" ], [ ("x", Past.Typ.TVar "a") ], Past.Expr.Identifier "x") );
    ]
    (parse_program
       (Lexing.from_string "let x: ['a; x: 'a]'a = box ('a; x: 'a |- x);;"))

let test_poly_box_alt_type_def _ =
  assert_equal
    [
      Past.TopLevelDefn.Definition
        ( ( "x",
            Past.Typ.TBox
              ([ "a" ], [ ("x", Past.Typ.TVar "a") ], Past.Typ.TVar "a") ),
          Past.Expr.Box
            ([ "a" ], [ ("x", Past.Typ.TVar "a") ], Past.Expr.Identifier "x") );
    ]
    (parse_program
       (Lexing.from_string "let x: ['a; x: 'a |- 'a] = box ('a; x: 'a |- x);;"))

let test_poly_box_use _ =
  assert_equal
    [
      Past.TopLevelDefn.Definition
        ( ( "x",
            Past.Typ.TBox
              ([ "a" ], [ ("x", Past.Typ.TVar "a") ], Past.Typ.TVar "a") ),
          Past.Expr.Box
            ([ "a" ], [ ("x", Past.Typ.TVar "a") ], Past.Expr.Identifier "x") );
      Past.TopLevelDefn.Expression
        (Past.Expr.LetBox
           ( "u",
             Past.Expr.Identifier "x",
             Past.Expr.Closure
               ( "u",
                 [ Past.Typ.TInt ],
                 [ Past.Expr.Constant (Past.Constant.Integer 1) ] ) ));
    ]
    (parse_program
       (Lexing.from_string
          "let x: ['a; x: 'a |- 'a] = box ('a; x: 'a |- x);;\n\
          \          let box u = x in\n\
          \              u with [int](1);;"))

let test_datatype_def_list _ =
  assert_equal
    [
      Past.TopLevelDefn.DatatypeDecl
        [
          ( [ "a" ],
            "list",
            [
              ("Nil", None);
              ( "Cons",
                Some
                  (Past.Typ.TProd
                     [
                       Past.Typ.TVar "a";
                       Past.Typ.TIdentifier ([ Past.Typ.TVar "a" ], "list");
                     ]) );
            ] );
        ];
    ]
    (parse_program
       (Lexing.from_string "datatype 'a list = Nil | Cons of ('a * 'a list);;"))

let test_datatype_def_sum _ =
  assert_equal
    [
      Past.TopLevelDefn.DatatypeDecl
        [
          ( [ "a"; "b" ],
            "sum",
            [
              ("Left", Some (Past.Typ.TVar "a"));
              ("Right", Some (Past.Typ.TVar "b"));
            ] );
        ];
    ]
    (parse_program
       (Lexing.from_string
          "datatype ('a, 'b) sum = Left of ('a) | Right of ('b);;"))

let test_constr_with_type_app _ =
  assert_equal
    [
      Past.TopLevelDefn.Expression
        (Past.Expr.Constr
           ( "Left",
             [ Past.Typ.TInt; Past.Typ.TString ],
             Some (Past.Expr.Constant (Past.Constant.Integer 1)) ));
      Past.TopLevelDefn.Expression
        (Past.Expr.Constr ("Nil", [ Past.Typ.TInt ], None));
      Past.TopLevelDefn.Expression
        (Past.Expr.Constr
           ( "Cons",
             [ Past.Typ.TInt ],
             Some
               (Past.Expr.Prod
                  [
                    Past.Expr.Constant (Past.Constant.Integer 2);
                    Past.Expr.Constr
                      ( "Cons",
                        [ Past.Typ.TInt ],
                        Some
                          (Past.Expr.Prod
                             [
                               Past.Expr.Constant (Past.Constant.Integer 1);
                               Past.Expr.Constr ("Nil", [ Past.Typ.TInt ], None);
                             ]) );
                  ]) ));
    ]
    (parse_program
       (Lexing.from_string
          "Left[int, string] 1;; Nil[int];; Cons[int] (2, Cons[int] (1, \
           Nil[int]));;"))

(* Name the test cases and group them together *)
let suite =
  "parsing_suite"
  >::: [
         "test_int" >:: test_int;
         "test_char" >:: test_char;
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
         "test_lift" >:: test_lift;
         "test_mutual_recursion_defn" >:: test_mutual_recursion_defn;
         "test_mutual_recursion_expr" >:: test_mutual_recursion_expr;
         "test_string" >:: test_string;
         "test_string_operators" >:: test_string_operators;
         "test_string_match" >:: test_string_match;
         "test_deref" >:: test_deref;
         "test_ref_type" >:: test_ref_type;
         "test_ref" >:: test_ref;
         "test_seq" >:: test_seq;
         "test_while" >:: test_while;
         "test_seq_precedence" >:: test_seq_precedence;
         "test_type_app" >:: test_type_app;
         "test_big_lambda" >:: test_big_lambda;
         "test_big_lambda_precedance_wrt_func"
         >:: test_big_lambda_precedance_wrt_func;
         "test_poly_box" >:: test_poly_box;
         "test_poly_box_defn" >:: test_poly_box_defn;
         "test_poly_box_alt_type_def" >:: test_poly_box_alt_type_def;
         "test_poly_box_use" >:: test_poly_box_use;
         "test_datatype_def_list" >:: test_datatype_def_list;
         "test_datatype_def_sum" >:: test_datatype_def_sum;
         "test_constr_with_type_app" >:: test_constr_with_type_app;
       ]

let () = run_test_tt_main suite
