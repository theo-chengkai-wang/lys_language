open Lys_ast

module type Context = sig
  type ('a, 'b) t [@@derive sexp, show]

  val empty_context : ('a, 'b) t
  val add_mapping : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  val delete_last_mapping : ('a, 'b) t -> 'a -> ('a, 'b) t
  val get_last_mapping : ('a, 'b) t -> 'a -> 'b option
end

module NaiveTypingContext : Context = struct
  type ('a, 'b) t = ('a * 'b) list [@@derive sexp, show]

  let empty_context : ('a, 'b) t = []
  let add_mapping ctx a b = (a, b) :: ctx

  let rec delete_last_mapping ctx a =
    match ctx with
    | [] -> []
    | (k, v) :: ctx ->
        if k = a then ctx else (k, v) :: delete_last_mapping ctx a

  let rec get_last_mapping ctx a =
    match ctx with
    | [] -> None
    | (k, v) :: ctx -> if k = a then Some v else get_last_mapping ctx a
end

let type_check_program program = ()
let type_check_program_aux meta_ctx ctx program = ()
let type_check_expression _ _ = ()
let type_inference_expression _ = Past.Typ.TUnit
let type_inference_directive _ = ()
