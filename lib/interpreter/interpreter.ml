open Lys_ast
open Core
open Lys_utils
open Lys_typing

(* Top level evaluation context *)
module EvaluationContext : sig
  type single_record = { typ : Ast.Typ.t; is_rec : bool; value : Ast.Value.t }
  [@@deriving show, sexp, compare, equal]

  type t = single_record String_map.t [@@deriving sexp, compare, equal]

  val set : t -> key:string -> data:single_record -> t
  val find_or_error : t -> string -> single_record Or_error.t
  val empty : t
  val show : t -> string

  val to_typing_obj_context :
    t -> (Ast.Typ.t) Typing_context.ObjTypingContext.t
end = struct
  type single_record = { typ : Ast.Typ.t; is_rec : bool; value : Ast.Value.t }
  [@@deriving show, sexp, compare, equal]

  type t = single_record String_map.t [@@deriving sexp, compare, equal]

  let set = String_map.set

  let find_or_error map key =
    match String_map.find map key with
    | Some v -> Ok v
    | None ->
        error "EvaluationContextError: Key not found." key [%sexp_of: string]

  let empty = String_map.empty

  let show v =
    v |> String_map.to_alist
    |> List.fold ~init:"" ~f:(fun acc (id, record) ->
           acc ^ Printf.sprintf "(%s, %s)" id (show_single_record record))
    |> fun str -> Printf.sprintf "[%s]" str

  let to_typing_obj_context v =
    v |> String_map.to_alist
    |> List.map ~f:(fun (id, record) ->
           (Ast.ObjIdentifier.of_string id, record.typ))
    |> Typing_context.ObjTypingContext.add_all_mappings
         (Typing_context.ObjTypingContext.create_empty_context ())
end
(*
   let reduce ~top_level_context ~expr = ()

   let evaluate_program program = [] *)

let rec multi_step_reduce ~top_level_context ~expr =
  let open Or_error.Monad_infix in
  match expr with
  | Ast.Expr.Identifier id ->
      (*No error if we're dealing with sth at top level context, otherwise error*)
      let index = Ast.ObjIdentifier.get_debruijn_index id in
      if not (Ast.DeBruijnIndex.equal index Ast.DeBruijnIndex.top_level) then
        (*Error*)
        error
          "EvaluationError: Can only evaluate a top level identifier. A non \
           top-level identifier \n\
          \    must have been substituted already." id
          [%sexp_of: Ast.ObjIdentifier.t]
      else
        let id_str = Ast.ObjIdentifier.get_name id in
        EvaluationContext.find_or_error top_level_context id_str
        >>= fun { typ; is_rec; value } ->
        if not is_rec then
          (*Substitution -- no need to worry about De Bruijn indices as there is no way they can go wrong*)
          Ok value
        else
          (* Recursion: handle once together. *)
          Ast.DeBruijnIndex.create 0 >>= fun debruijn_index ->
          multi_step_reduce ~top_level_context
            ~expr:
              (Ast.Expr.LetRec
                 ( (Ast.ObjIdentifier.of_string id_str, typ),
                   Ast.Value.to_expr value,
                   Ast.Expr.Identifier
                     (Ast.ObjIdentifier.of_string_and_index id_str
                        debruijn_index) ))
  | Ast.Expr.Constant c -> Ok (Ast.Value.Constant c)
  | Ast.Expr.UnaryOp (op, expr) -> (
      multi_step_reduce ~top_level_context ~expr >>= fun v ->
      match (op, v) with
      | Ast.UnaryOperator.NEG, Ast.Value.Constant (Ast.Constant.Integer i) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (-i)))
      | Ast.UnaryOperator.NOT, Ast.Value.Constant (Ast.Constant.Boolean b) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (not b)))
      | _, _ ->
          error
            "EvaluationError: type mismatch at Unary operator. [FATAL] should \
             not happen! Type check should have prevented this."
            (Ast.Expr.UnaryOp (op, expr))
            [%sexp_of: Ast.Expr.t])
  | Ast.Expr.BinaryOp (op, expr1, expr2) -> (
      multi_step_reduce ~top_level_context ~expr:expr1 >>= fun v1 ->
      multi_step_reduce ~top_level_context ~expr:expr2 >>= fun v2 ->
      match (op, v1, v2) with
      | ( Ast.BinaryOperator.ADD,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 + i2)))
      | ( Ast.BinaryOperator.SUB,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 - i2)))
      | ( Ast.BinaryOperator.MUL,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 * i2)))
      | ( Ast.BinaryOperator.DIV,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 / i2)))
      | ( Ast.BinaryOperator.MOD,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Integer (i1 % i2)))
      | ( Ast.BinaryOperator.EQ,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 = i2)))
      | ( Ast.BinaryOperator.NEQ,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 <> i2)))
      | ( Ast.BinaryOperator.GTE,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 >= i2)))
      | ( Ast.BinaryOperator.GT,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 > i2)))
      | ( Ast.BinaryOperator.LT,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 < i2)))
      | ( Ast.BinaryOperator.LTE,
          Ast.Value.Constant (Ast.Constant.Integer i1),
          Ast.Value.Constant (Ast.Constant.Integer i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 <= i2)))
      | ( Ast.BinaryOperator.AND,
          Ast.Value.Constant (Ast.Constant.Boolean i1),
          Ast.Value.Constant (Ast.Constant.Boolean i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 && i2)))
      | ( Ast.BinaryOperator.OR,
          Ast.Value.Constant (Ast.Constant.Boolean i1),
          Ast.Value.Constant (Ast.Constant.Boolean i2) ) ->
          Ok (Ast.Value.Constant (Ast.Constant.Boolean (i1 || i2)))
      | _, _, _ ->
          error
            "EvaluationError: type mismatch at Binary operator. [FATAL] should \
             not happen! Type check should have prevented this."
            (Ast.Expr.BinaryOp (op, expr1, expr2))
            [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Prod (expr1, expr2) ->
      multi_step_reduce ~top_level_context ~expr:expr1 >>= fun v1 ->
      multi_step_reduce ~top_level_context ~expr:expr2 >>= fun v2 ->
      Ok (Ast.Value.Prod (v1, v2))
  | Ast.Expr.Fst expr -> (
      multi_step_reduce ~top_level_context ~expr >>= fun v ->
      match v with
      | Ast.Value.Prod (v1, _) -> Ok v1
      | _ ->
          error
            "EvaluationError: type mismatch at Fst. [FATAL] should not happen! \
             Type check should have prevented this."
            (Ast.Expr.Fst expr) [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Snd expr -> (
      multi_step_reduce ~top_level_context ~expr >>= fun v ->
      match v with
      | Ast.Value.Prod (_, v2) -> Ok v2
      | _ ->
          error
            "EvaluationError: type mismatch at Snd. [FATAL] should not happen! \
             Type check should have prevented this."
            (Ast.Expr.Snd expr) [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Left (t1, t2, expr) ->
      multi_step_reduce ~top_level_context ~expr >>= fun v ->
      Ok (Ast.Value.Left (t1, t2, v))
      (* We don't run RT type check here. *)
  | Ast.Expr.Right (t1, t2, expr) ->
      multi_step_reduce ~top_level_context ~expr >>= fun v ->
      Ok (Ast.Value.Right (t1, t2, v))
      (* We don't run RT type check here. *)
  | Ast.Expr.Case (e, (id1, _), e1, (id2, _), e2) ->
      multi_step_reduce ~top_level_context ~expr:e >>= fun v ->
      let new_expr_or_error =
        match v with
        | Ast.Value.Left (_, _, v) ->
            Substitutions.substitute (Ast.Value.to_expr v) id1 e1
        | Ast.Value.Right (_, _, v) ->
            Substitutions.substitute (Ast.Value.to_expr v) id2 e2
        | _ ->
            error
              "EvaluationError: type mismatch at match clause. [FATAL] should \
               not happen! Type check should have prevented this."
              expr [%sexp_of: Ast.Expr.t]
      in
      new_expr_or_error >>= fun new_expr ->
      multi_step_reduce ~top_level_context ~expr:new_expr
  | Ast.Expr.Lambda (iddef, e) -> Ok (Ast.Value.Lambda (iddef, e))
  | Ast.Expr.Application (e1, e2) -> (
      multi_step_reduce ~top_level_context ~expr:e1 >>= fun v1 ->
      multi_step_reduce ~top_level_context ~expr:e2 >>= fun v2 ->
      match v1 with
      | Ast.Value.Lambda ((id, _), e) ->
          Substitutions.substitute (Ast.Value.to_expr v2) id e
          >>= fun new_expr ->
          multi_step_reduce ~top_level_context ~expr:new_expr
      | _ ->
          error
            "EvaluationError: type mismatch at Lambda. [FATAL] should not \
             happen! Type check should have prevented this."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.IfThenElse (b, e1, e2) -> (
      multi_step_reduce ~top_level_context ~expr:b >>= fun bv ->
      match bv with
      | Ast.Value.Constant (Ast.Constant.Boolean v) ->
          if v then multi_step_reduce ~top_level_context ~expr:e1
          else multi_step_reduce ~top_level_context ~expr:e2
      | _ ->
          error
            "EvaluationError: type mismatch at IfThenElse. [FATAL] should not \
             happen! Type check should have prevented this."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.LetBinding ((id, _), e, e2) ->
      multi_step_reduce ~top_level_context ~expr:e >>= fun ev ->
      Substitutions.substitute (Ast.Value.to_expr ev) id e2 >>= fun e ->
      multi_step_reduce ~top_level_context ~expr:e
  | Ast.Expr.LetRec ((id, typ), e, e2) ->
      (* let rec f = v in e  ~~~~~> [([(let rec f = v in f)/f]v)/f]e

         Analysis
           let rec f = (0)
               v (1 -- means that index 0 at top level = f)
           in
               e (1)

           (fun f (0)-> e(1)) ((fun f (0) -> v(1)) (let rec f(0) = v(1) in f(1)))

         So Shifting is not really needed!
         However, I can't just translate it to the above, for it will easily stack overflow, in a very unfortunate way,
         do to my call by value semantics. Instead, I shall have to actually manually write the substitutions out.
      *)
      multi_step_reduce ~top_level_context ~expr:e >>= fun v ->
      Ast.DeBruijnIndex.create 0 >>= fun db_index_0 ->
      (*let v1 = [(let rec f = v in f)/f]v*)
      let ev = Ast.Value.to_expr v in
      let to_sub_in =
        Ast.Expr.LetRec
          ( (id, typ),
            ev,
            Ast.Expr.Identifier
              (Ast.ObjIdentifier.of_string_and_index
                 (Ast.ObjIdentifier.get_name id)
                 db_index_0) )
      in
      Substitutions.substitute to_sub_in id ev >>= fun ev1 ->
      (*[v1/f]e*)
      Substitutions.substitute ev1 id e2 >>= fun ev2 ->
      multi_step_reduce ~top_level_context ~expr:ev2
  | Ast.Expr.Box (ctx, e) -> Ok (Ast.Value.Box (ctx, e))
  | Ast.Expr.LetBox (metaid, e, e2) -> (
      multi_step_reduce ~top_level_context ~expr:e >>= fun box_v ->
      match box_v with
      | Ast.Value.Box (ctx, e_box) ->
          Substitutions.meta_substitute ctx e_box metaid e2
          >>= fun res_meta_sub ->
          multi_step_reduce ~top_level_context ~expr:res_meta_sub
      | _ ->
          error
            "EvaluationError: type mismatch at LetBox. [FATAL] should not \
             happen! Type check should have prevented this."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Closure (_, _) ->
      error "EvaluationError: One should never have to evaluate a raw closure."
        expr [%sexp_of: Ast.Expr.t]

(*
   | Ast.Expr.Identifier id -> Ok ()
   | Ast.Expr.Constant c -> Ok ()
   | Ast.Expr.UnaryOp (op, expr) -> Ok ()
   | Ast.Expr.BinaryOp (op, expr, expr2) -> Ok ()
   | Ast.Expr.Prod (expr1, expr2) -> Ok ()
   | Ast.Expr.Fst expr -> Ok ()
   | Ast.Expr.Snd expr -> Ok ()
   | Ast.Expr.Left (t1, t2, expr) -> Ok ()
   | Ast.Expr.Right (t1, t2, expr) -> Ok ()
   | Ast.Expr.Match (e, iddef1, e1, iddef2, e2) -> Ok ()
   | Ast.Expr.Lambda (iddef, e) -> Ok ()
   | Ast.Expr.Application (e1, e2) -> Ok ()
   | Ast.Expr.IfThenElse (b, e1, e2) -> Ok ()
   | Ast.Expr.LetBinding (iddef, e, e2) -> Ok ()
   | Ast.Expr.LetRec (iddef, e, e2) -> Ok ()
   | Ast.Expr.Box (ctx, e) -> Ok ()
   | Ast.Expr.LetBox (metaid, e, e2) -> Ok ()
   | Ast.Expr.Closure (metaid, exprs) -> Ok ()
*)

module TopLevelEvaluationResult = struct
  type t =
    | ExprValue of Ast.Typ.t * Ast.Value.t
    | Defn of Ast.IdentifierDefn.t * Ast.Value.t
    | RecDefn of Ast.IdentifierDefn.t * Ast.Value.t
    | Directive of Ast.Directive.t * string
  [@@deriving sexp, compare, equal, show]

  let get_str_output = function
    | ExprValue (typ, v) ->
        Printf.sprintf "val:\n\t%s \n=\n\t %s" (Ast.Typ.show typ)
          (Ast.Value.show v)
    | Defn ((id, typ), v) ->
        Printf.sprintf "val %s :\n\t%s \n=\n\t %s"
          (Ast.ObjIdentifier.get_name id)
          (Ast.Typ.show typ) (Ast.Value.show v)
    | RecDefn ((id, typ), v) ->
        Printf.sprintf "val rec %s:\n\t %s \n=\n\t %s"
          (Ast.ObjIdentifier.get_name id)
          (Ast.Typ.show typ) (Ast.Value.show v)
    | Directive (d, message) ->
        Printf.sprintf
          "Executed directive %s\n\tMessage from the directive:\n\t %s"
          (Ast.Directive.show d) message
end

let evaluate_top_level_defn ?(top_level_context = EvaluationContext.empty)
    top_level_defn =
  let open Or_error.Monad_infix in
  match top_level_defn with
  | Ast.TypedTopLevelDefn.Definition (typ, (id, _), e) ->
      multi_step_reduce ~top_level_context ~expr:e >>= fun v ->
      let new_context =
        EvaluationContext.set top_level_context
          ~key:(Ast.ObjIdentifier.get_name id)
          ~data:{ is_rec = false; typ; value = v }
      in
      Ok (TopLevelEvaluationResult.Defn ((id, typ), v), new_context)
  | Ast.TypedTopLevelDefn.RecursiveDefinition (typ, (id, _), e) ->
      multi_step_reduce ~top_level_context ~expr:e >>= fun v ->
      let new_entry : EvaluationContext.single_record =
        { is_rec = true; typ; value = v }
      in
      let key = Ast.ObjIdentifier.get_name id in
      let new_context =
        EvaluationContext.set top_level_context ~key ~data:new_entry
      in
      Ok (TopLevelEvaluationResult.RecDefn ((id, typ), v), new_context)
  | Ast.TypedTopLevelDefn.Expression (typ, e) ->
      multi_step_reduce ~top_level_context ~expr:e >>= fun v ->
      Ok (TopLevelEvaluationResult.ExprValue (typ, v), top_level_context)
  | Ast.TypedTopLevelDefn.Directive d -> (
      match d with
      | Ast.Directive.Env ->
          let env = EvaluationContext.show top_level_context in
          let message = Printf.sprintf "ENV: \n\t%s" env in
          Ok (TopLevelEvaluationResult.Directive (d, message), top_level_context)
      | Ast.Directive.Quit ->
          Ok
            ( TopLevelEvaluationResult.Directive (d, "User Quit."),
              top_level_context )
      | Ast.Directive.Reset ->
          let message = "Cleared Env" in
          let new_env = EvaluationContext.empty in
          Ok (TopLevelEvaluationResult.Directive (d, message), new_env))

let rec evaluate_top_level_defns ?(top_level_context = EvaluationContext.empty)
    program =
  let open Or_error.Monad_infix in
  match program with
  | [] -> Ok ([], top_level_context)
  | top :: tops -> (
      evaluate_top_level_defn ~top_level_context top
      >>= fun (top_level_result, new_context) ->
      match top_level_result with
      | TopLevelEvaluationResult.Directive (Ast.Directive.Quit, _) ->
          Ok ([ top_level_result ], new_context)
      | _ ->
          evaluate_top_level_defns ~top_level_context:new_context tops
          >>= fun (evaluation_res, new_context) ->
          Ok (top_level_result :: evaluation_res, new_context))

let evaluate_program ?(top_level_context = EvaluationContext.empty) program =
  let open Or_error.Monad_infix in
  evaluate_top_level_defns ~top_level_context program
  >>= fun (evaluation_res, _) -> Ok evaluation_res
