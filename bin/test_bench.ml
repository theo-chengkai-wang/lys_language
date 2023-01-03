(* let () = print_endline "Hello, World!" *)
(* My test bench *)

open Core
open Lys_parsing
open Lys_typing
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
  let current_meta_identifiers =
    String_map.empty |> String_map.set ~key:"u" ~data:0
  in
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
    Substitutions.meta_substitute
      [ (Ast.ObjIdentifier.of_string "A", Ast.Typ.TInt) ]
      (Ast.Expr.Identifier (Ast.ObjIdentifier.of_string_and_index "A" db_index))
      (Ast.MetaIdentifier.of_string "u")
      expr)
  |> ok_exn |> Ast.Expr.show |> print_endline

let loop2 str () =
  print_endline "";
  ( str |> Lexing.from_string |> Lex_and_parse.parse_program
    |> Ast.Program.of_past
       (* |> Ast.Program.of_past
          |> Ast.TypedProgram.convert_from_untyped_without_typecheck *)
    |> Typecore.type_check_program |> ok_exn
  |> fun typed_program ->
    match List.hd_exn typed_program with
    | Ast.TypedTopLevelDefn.Expression (typ, e) ->
        print_endline
          (Printf.sprintf "Type of Program is %s" (Ast.Typ.show typ));
        e
    | _ -> failwith "Not supposed to be here" )
  |> fun e ->
  Interpreter.multi_step_reduce
    ~top_level_context:Interpreter.EvaluationContext.empty ~expr:e
  |> ok_exn |> Ast.Value.show
  |> fun s -> print_endline (Printf.sprintf "Run result of expression:\n %s" s)

let loop3 str () =
  print_endline "";
  str |> Lexing.from_string |> Lex_and_parse.parse_program
  (* |> Ast.Program.of_past
     |> Ast.TypedProgram.convert_from_untyped_without_typecheck *)
  |> Ast.Program.of_past
  |> Typecore.type_check_program |> ok_exn |> Interpreter.evaluate_program
  |> ok_exn
  |> fun l ->
  let _ =
    List.map l ~f:(fun res ->
        print_endline (Interpreter.TopLevelEvaluationResult.get_str_output res);
        print_endline "")
  in
  ()

let program =
  "\n\
   let rec (pow: int -> int -> int) = fun (n:int) -> if n = 0 then (fun \
   (x:int) -> 1) else (fun (x:int) -> x * (pow (n-1) x)) in pow 2;;\n"

let program2 =
  "let rec (pow: int -> [](int -> int)) = \n\
  \  fun (n:int) -> if n = 0 then box (|- fun (b:int)->1) else \n\
  \  (let box u = pow (n-1) in box (|- fun (b:int) -> b * ((u with ()) b))) in \
   pow 2;;"

let program3 =
  "let rec (pow: int -> [b:int]int) = fun (n:int) -> if n = 0 then box (b:int \
   |- 1) else let box u = pow (n-1) in box (b:int |- b * (u with (b)))\n\
  \  in pow 2;;"

let program4 =
  "let rec (pow: int -> int -> int) = fun (n:int) -> if n = 0 then (fun 
   (x:int) -> 1) else (fun (x:int) -> x * (pow (n-1) x));;
  pow 2 3;;
  ENV;;
  RESET;;
  let rec (pow: int -> [b:int]int) = fun (n:int) -> if n = 0 then box 
   (b:int |- 1) else let box u = pow (n-1) in box (b:int |- b * (u with (b)));;
  pow 2;;"

(* let program5 = "let rec " *)
let () = loop3 program4 ()
