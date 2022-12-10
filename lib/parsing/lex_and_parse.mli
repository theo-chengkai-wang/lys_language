val parse_expression : Lexing.lexbuf -> Lys_ast.Ast.Expr.t option
val parse_and_print: Lexing.lexbuf -> unit
val parse_program: Lexing.lexbuf -> Lys_ast.Ast.Program.t