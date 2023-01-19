open Lys_ast
open Core
open Interpreter_common

type t =
  | MultiStep of { show_time : bool }
  (*Show Time and Show Steps*)
  | SingleStep of { show_step_count : bool; verbose : bool }
(*Show Steps and Verbose*)
[@@deriving show, sexp]

let arg_type =
  Command.Arg_type.create (fun s ->
      match s with
      | "m" -> MultiStep { show_time = false }
      | "mt" -> MultiStep { show_time = true }
      | "s" -> SingleStep { show_step_count = false; verbose = false }
      | "ss" -> SingleStep { show_step_count = true; verbose = false }
      | "ssv" -> SingleStep { show_step_count = true; verbose = true }
      | _ -> failwith "Invalid interpreter choice.")

let rec evaluate_top_level_defns ?(top_level_context = EvaluationContext.empty)
    ?(type_constr_context = TypeConstrContext.empty)
    ?(interpreter = MultiStep { show_time = false }) program =
  let open Or_error.Monad_infix in
  match program with
  | [] -> Ok ([], top_level_context, type_constr_context)
  | top :: tops -> (
      (match interpreter with
      | MultiStep { show_time } ->
          Multi_step.evaluate_top_level_defn ~top_level_context
            ~type_constr_context ~time_exec:show_time top
      | SingleStep { show_step_count; verbose } ->
          Single_step.evaluate_top_level_defn top ~top_level_context
            ~type_constr_context ~show_step_count ~verbose)
      >>= fun (top_level_result, new_context, new_typ_context) ->
      match top_level_result with
      | TopLevelEvaluationResult.Directive (Ast.Directive.Quit, _) ->
          Ok ([ top_level_result ], new_context, new_typ_context)
      | _ ->
          evaluate_top_level_defns ~top_level_context:new_context ~interpreter
            ~type_constr_context:new_typ_context tops
          >>= fun (evaluation_res, new_context, new_typ_context) ->
          Ok (top_level_result :: evaluation_res, new_context, new_typ_context))

let evaluate_program ?(top_level_context = EvaluationContext.empty)
    ?(type_constr_context = TypeConstrContext.empty)
    ?(interpreter = MultiStep { show_time = false }) program =
  let open Or_error.Monad_infix in
  evaluate_top_level_defns ~top_level_context ~type_constr_context ~interpreter
    program
  >>= fun (evaluation_res, _, _) -> Ok evaluation_res
