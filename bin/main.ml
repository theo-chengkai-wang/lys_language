(* let () = print_endline "Hello, World!" *)
(* My test bench *)

open Core
open Lys_parsing

(* open Lys_typing *)
open Lys_utils
open Lys_ast
open Lys_interpreter 

let loop str () =
  ( str |> Lexing.from_string |> Lex_and_parse.parse_program
    |> Ast.Program.of_past
    |> Ast.TypedProgram.convert_from_untyped_without_typecheck
  |> fun typed_program ->
    match List.hd_exn typed_program with
    | Ast.TypedTopLevelDefn.Expression (_, e) -> e
    | _ -> failwith "Not supposed to be here" )
  |> fun e ->
  let current_identifiers =
    String_map.empty |> String_map.set ~key:"y" ~data:0
  in
  let current_meta_identifiers = String_map.empty |> String_map.set ~key:"u" ~data:0 in
  Ast.Expr.populate_index e ~current_ast_level:1 ~current_meta_ast_level:1
    ~current_identifiers ~current_meta_identifiers
  |> ok_exn
  |> (*Do something*)
  (fun expr ->
    let open Or_error.Monad_infix in
    Ast.DeBruijnIndex.create 0 >>= fun db_index ->
    (* Substitutions.substitute
      (Ast.Expr.Identifier (Ast.ObjIdentifier.of_string_and_index "w" db_index))
      (Ast.ObjIdentifier.of_string "y")
      expr) *)
    Substitutions.meta_substitute [(Ast.ObjIdentifier.of_string "A", Ast.Typ.TInt)] (Ast.Expr.Identifier (Ast.ObjIdentifier.of_string_and_index "A" db_index)) (Ast.MetaIdentifier.of_string "u") expr )
  |> ok_exn |> 
  Ast.Expr.show |> print_endline

let () = loop "let rec (x:int -> int) = fun (z:int) -> if z = 0 then 0 else y + (u with (100000)) + x (z-1) in x 1;;" ()
