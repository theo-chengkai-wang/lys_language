val type_check_program: Lys_ast.Ast.program -> unit
val type_check_expression: Lys_ast.Ast.typ -> Lys_ast.Ast.expression -> unit
val type_inference_expression: Lys_ast.Ast.expression -> Lys_ast.Ast.typ
