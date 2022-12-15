open Core
open OUnit2
open Lys_ast
open Lys_typing

let read_parse_file_and_do filename f =
  In_channel.with_file filename ~f:(fun file_ic ->
      f (Lys_parsing.Lex_and_parse.parse_program (Lexing.from_channel file_ic)))

let prefix =
  Filename.concat Filename.current_dir_name
    (Filename.concat "example_programs" "cmtt_paper_proofs")

let test_m1 _ =
  let filename = Filename.concat prefix "m1.lys" in
  let result = read_parse_file_and_do filename Typecore.type_check_program in
  assert_equal (Or_error.ok result)
    (Some
    [(Ast.TypedTopLevelDefn.Expression (Ast.Typ.TUnit,
    (Ast.Expr.LetBinding (
       (Ast.ObjIdentifier.of_string "m1",
        (Ast.Typ.TFun (
           (Ast.Typ.TBox ([(Ast.ObjIdentifier.of_string "x", (Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "C")))],
              (Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "A")))),
           (Ast.Typ.TBox (
              [((Ast.ObjIdentifier.of_string "y1"), (Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "C")));
                ((Ast.ObjIdentifier.of_string "y2"), (Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "D")))],
              (Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "A"))))
           ))),
       (Ast.Expr.Lambda (
          ((Ast.ObjIdentifier.of_string "z"),
           (Ast.Typ.TBox ([(Ast.ObjIdentifier.of_string "x", (Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "C")))],
              (Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "A"))))),
          (Ast.Expr.LetBox ((Ast.MetaIdentifier.of_string "u"), (Ast.Expr.Identifier (Ast.ObjIdentifier.of_string "z")),
             (Ast.Expr.Box (
                [((Ast.ObjIdentifier.of_string "y1"), (Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "C")));
                  ((Ast.ObjIdentifier.of_string "y2"), (Ast.Typ.TIdentifier (Ast.TypeIdentifier.of_string "D")))],
                (Ast.Expr.Closure ((Ast.MetaIdentifier.of_string "u"), [(Ast.Expr.Identifier (Ast.ObjIdentifier.of_string "y1"))]))))
             ))
          )),
       (Ast.Expr.Constant Ast.Constant.Unit)))
    ))
  ] )

let suite = "suite" >::: [ "test_m1" >:: test_m1 ]
