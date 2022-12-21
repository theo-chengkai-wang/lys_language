exception SyntaxError of string 
exception ParsingError of string

val parse_expression : Lexing.lexbuf -> Lys_ast.Past.Expr.t option
val parse_and_print: Lexing.lexbuf -> unit
val parse_program: Lexing.lexbuf -> Lys_ast.Past.Program.t
