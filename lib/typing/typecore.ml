open Core
open Lys_ast

(*
  TODO: Maybe instead of doing 1 pass, do 2 passes to distinguish Past->Ast and Type checking AST.   
*)

(* meta_ctx->ctx->Aast.Expr.t -> Ast.Typ.t -> unit Or_error.t *)

module BoxContextSet = Set.Make (Ast.ObjIdentifier)

let box_context_contains_duplicates ctx =
  (*Return true if there are duplicates*)
  let rec check_duplicates_aux map ctx =
    match ctx with
    | [] -> false
    | (x, _) :: xs ->
        if BoxContextSet.mem map x then true
        else check_duplicates_aux (BoxContextSet.add map x) xs
  in
  check_duplicates_aux BoxContextSet.empty ctx

let rec type_check_expression meta_ctx ctx expr typ =
  let open Or_error.Monad_infix in
  type_inference_expression meta_ctx ctx expr >>= fun inferred_typ ->
  if Ast.Typ.equal typ inferred_typ then Ok ()
  else
    error
      (Printf.sprintf
         "TypeCheckError: Inferred type %s\n Not Equal to checked type %s."
         (Ast.Typ.show inferred_typ)
         (Ast.Typ.show typ))
      (expr, inferred_typ, typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t * Ast.Typ.t]

and type_inference_expression meta_ctx ctx e =
  let open Or_error.Monad_infix in
  match e with
  | Ast.Expr.Identifier id -> (
      let res = Typing_context.ObjTypingContext.get_last_mapping ctx id in
      match res with
      | None ->
          error
            ("TypeInferenceError: Unbound or unknown identifier: "
           ^ Ast.ObjIdentifier.show id)
            (ctx, id)
            [%sexp_of:
              Ast.Typ.t Typing_context.ObjTypingContext.t * Ast.ObjIdentifier.t]
      | Some typ -> Ok typ)
  | Ast.Expr.Constant c -> (
      match c with
      | Ast.Constant.Boolean _ -> Ok Ast.Typ.TBool
      | Ast.Constant.Integer _ -> Ok Ast.Typ.TInt
      | Ast.Constant.Unit -> Ok Ast.Typ.TUnit)
  | Ast.Expr.UnaryOp (op, expr) -> (
      match op with
      | Ast.UnaryOperator.NEG ->
          type_check_expression meta_ctx ctx expr Ast.Typ.TInt >>= fun () ->
          Ok Ast.Typ.TInt
      | Ast.UnaryOperator.NOT ->
          type_check_expression meta_ctx ctx expr Ast.Typ.TBool >>= fun () ->
          Ok Ast.Typ.TBool)
  | Ast.Expr.BinaryOp (op, expr, expr2) -> (
      let check_both typ_to_check ok_typ =
        type_check_expression meta_ctx ctx expr typ_to_check >>= fun () ->
        type_check_expression meta_ctx ctx expr2 typ_to_check >>= fun () ->
        Ok ok_typ
      in
      match op with
      | Ast.BinaryOperator.ADD -> check_both Ast.Typ.TInt Ast.Typ.TInt
      | Ast.BinaryOperator.SUB -> check_both Ast.Typ.TInt Ast.Typ.TInt
      | Ast.BinaryOperator.MUL -> check_both Ast.Typ.TInt Ast.Typ.TInt
      | Ast.BinaryOperator.DIV -> check_both Ast.Typ.TInt Ast.Typ.TInt
      | Ast.BinaryOperator.MOD -> check_both Ast.Typ.TInt Ast.Typ.TInt
      | Ast.BinaryOperator.EQ ->
          (*
        Infer one and check the other one
        Improve error message saying "both sides of equality must be of same type"   
      *)
          type_inference_expression meta_ctx ctx expr >>= fun typ ->
          let or_error = type_check_expression meta_ctx ctx expr2 typ in
          Or_error.tag or_error
            ~tag:
              ("TypeInferenceError: Type mismatch: both sides of equality must \
                have same type: " ^ Ast.Typ.show typ)
          >>= fun _ -> Ok Ast.Typ.TBool
      | Ast.BinaryOperator.NEQ ->
          type_inference_expression meta_ctx ctx expr >>= fun typ ->
          let or_error = type_check_expression meta_ctx ctx expr2 typ in
          Or_error.tag or_error
            ~tag:
              ("TypeInferenceError: Type mismatch: both sides of inequality \
                must have same type: " ^ Ast.Typ.show typ)
          >>= fun _ -> Ok Ast.Typ.TBool
      | Ast.BinaryOperator.GTE -> check_both Ast.Typ.TInt Ast.Typ.TBool
      | Ast.BinaryOperator.GT -> check_both Ast.Typ.TInt Ast.Typ.TBool
      | Ast.BinaryOperator.LTE -> check_both Ast.Typ.TInt Ast.Typ.TBool
      | Ast.BinaryOperator.LT -> check_both Ast.Typ.TInt Ast.Typ.TBool
      | Ast.BinaryOperator.AND -> check_both Ast.Typ.TBool Ast.Typ.TBool
      | Ast.BinaryOperator.OR -> check_both Ast.Typ.TBool Ast.Typ.TBool)
  | Ast.Expr.Prod (expr1, expr2) ->
      type_inference_expression meta_ctx ctx expr1 >>= fun typ ->
      type_inference_expression meta_ctx ctx expr2 >>= fun typ2 ->
      Ok (Ast.Typ.TProd (typ, typ2))
  | Ast.Expr.Fst expr -> (
      type_inference_expression meta_ctx ctx expr >>= fun typ ->
      match typ with
      | Ast.Typ.TProd (typ1, _) -> Ok typ1
      | _ ->
          (*ERROR*)
          error "TypeInferenceError: Argument of fst must be of product type."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Snd expr -> (
      type_inference_expression meta_ctx ctx expr >>= fun typ ->
      match typ with
      | Ast.Typ.TProd (_, typ2) -> Ok typ2
      | _ ->
          (*ERROR*)
          error "TypeInferenceError: Argument of snd must be of product type."
            expr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.Left (t1, t2, expr) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx expr t1)
        ~tag:
          ("TypeInferenceError: the given expression must be of the left type \
            of the given sum type: " ^ Ast.Typ.show t1)
      >>= fun () -> Ok (Ast.Typ.TSum (t1, t2))
  | Ast.Expr.Right (t1, t2, expr) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx expr t2)
        ~tag:
          ("TypeInferenceError: the given expression must be of of the right \
            type of the given sum type: " ^ Ast.Typ.show t2)
      >>= fun () -> Ok (Ast.Typ.TSum (t1, t2))
  | Ast.Expr.Case (e, iddef1, e1, iddef2, e2) ->
      type_inference_expression meta_ctx ctx e >>= fun e_typ ->
      (match e_typ with
      | Ast.Typ.TSum (_, _) -> Ok ()
      | typ ->
          error "TypeInferenceError: argument of `match` must be of a sum type."
            typ [%sexp_of: Ast.Typ.t])
      >>= fun () ->
      let id1, typ1 = iddef1 in
      let new_ctx1 = Typing_context.ObjTypingContext.add_mapping ctx id1 typ1 in
      type_inference_expression meta_ctx new_ctx1 e1 >>= fun e1_type ->
      let id2, typ2 = iddef2 in
      let new_ctx2 = Typing_context.ObjTypingContext.add_mapping ctx id2 typ2 in
      (*Types should match, or there is a Type mismatch error*)
      let or_error = type_check_expression meta_ctx new_ctx2 e2 e1_type in
      Or_error.tag or_error
        ~tag:
          "TypeInferenceError: Type mismatch: Each outcome of a match clause \
           must give the same type"
      >>= fun () -> Ok e1_type
  | Ast.Expr.Lambda ((id, typ), e) ->
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      type_inference_expression meta_ctx new_ctx e >>= fun e_typ ->
      Ok (Ast.Typ.TFun (typ, e_typ))
  | Ast.Expr.Application (e1, e2) ->
      type_inference_expression meta_ctx ctx e1 >>= fun e1_typ ->
      (match e1_typ with
      | Ast.Typ.TFun (arg_typ, res_typ) -> Ok (arg_typ, res_typ)
      | actual_typ ->
          error
            "TypeInferenceError: only functions can be applied; the expression \
             given is not a function."
            actual_typ [%sexp_of: Ast.Typ.t])
      >>= fun (arg_typ, res_typ) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx e2 arg_typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: Type mismatch: expected argument of type %s."
             (Ast.Typ.show arg_typ))
      >>= fun () -> Ok res_typ
  | Ast.Expr.IfThenElse (b, e1, e2) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx b Ast.Typ.TBool)
        ~tag:
          "TypeInferenceError: the predicate of an if-then-else expression \
           must be of boolean type"
      >>= fun () ->
      type_inference_expression meta_ctx ctx e1 >>= fun e1_typ ->
      Or_error.tag_arg
        (type_check_expression meta_ctx ctx e2 e1_typ)
        "TypeInferenceError: All outcomes of an if-then-else expression must \
         be of the same type."
        (e2, e1_typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t]
      >>= fun () -> Ok e1_typ
  | Ast.Expr.LetBinding ((id, typ), e, e2) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx e typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: variable %s is declared of type %s but bound \
              to an expression of a different type"
             (Ast.ObjIdentifier.show id)
             (Ast.Typ.show typ))
      >>= fun () ->
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      type_inference_expression meta_ctx new_ctx e2
  | Ast.Expr.LetRec ((id, typ), e, e2) ->
      (*We only allow types which are of the form A->B*)
      (match typ with
      | Ast.Typ.TFun (_, _) -> Ok ()
      | _ ->
          error
            "TypeInferenceError: let rec declaration must have function type."
            () [%sexp_of: unit])
      >>= fun () ->
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      Or_error.tag
        (type_check_expression meta_ctx new_ctx e typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: recursive variable %s is declared of type %s \
              but bound to an expression of a different type"
             (Ast.ObjIdentifier.show id)
             (Ast.Typ.show typ))
      >>= fun () -> type_inference_expression meta_ctx new_ctx e2
  | Ast.Expr.Box (context, e) ->
      (if box_context_contains_duplicates context then
       error
         "TypeInferenceError: there are duplicates in the provided box context"
         context [%sexp_of: Ast.Context.t]
      else Ok ())
      >>= fun () ->
      let new_ctx =
        Typing_context.ObjTypingContext.add_all_mappings ctx context
      in
      type_inference_expression meta_ctx new_ctx e >>= fun e_typ ->
      Ok (Ast.Typ.TBox (context, e_typ))
  | Ast.Expr.LetBox (metaid, e, e2) ->
      ( (*First check that e gives a box*)
        type_inference_expression meta_ctx ctx e
      >>= fun e_typ ->
        match e_typ with
        | Ast.Typ.TBox (box_context, box_typ) -> Ok (box_context, box_typ)
        | _ ->
            Or_error.error "TypeInferenceError: Can only unbox a boxed term."
              (metaid, e, e_typ)
              [%sexp_of: Ast.MetaIdentifier.t * Ast.Expr.t * Ast.Typ.t] )
      >>= fun (box_context, box_typ) ->
      (*Add it to the meta context and continue*)
      let new_meta_ctx =
        Typing_context.MetaTypingContext.add_mapping meta_ctx metaid
          (box_context, box_typ)
      in
      type_inference_expression new_meta_ctx ctx e2
  | Ast.Expr.Closure (meta_id, exprs) ->
      let meta_var_option =
        Typing_context.MetaTypingContext.get_last_mapping meta_ctx meta_id
      in
      (match meta_var_option with
      | None ->
          (*Error*)
          error
            ("TypeInferenceError: unbound meta identifier"
            ^ Ast.MetaIdentifier.show meta_id)
            (meta_ctx, meta_id)
            [%sexp_of:
              (Ast.Context.t * Ast.Typ.t) Typing_context.MetaTypingContext.t
              * Ast.MetaIdentifier.t]
      | Some (box_context, box_typ) -> Ok (box_context, box_typ))
      >>= fun (box_context, box_typ) ->
      (*Check expressions match box_context*)
      (*1- check length*)
      let zipped_list_option = List.zip box_context exprs in
      (match zipped_list_option with
      | List.Or_unequal_lengths.Ok zipped_list -> Ok zipped_list
      | List.Or_unequal_lengths.Unequal_lengths ->
          error
            "TypeInferenceError: the number of arguments to the `with` \
             expression does not match the size of the context"
            (exprs, box_context) [%sexp_of: Ast.Expr.t list * Ast.Context.t])
      >>= fun zipped_list ->
      (*2- check types*)
      Or_error.tag
        (Or_error.combine_errors_unit
           (List.map zipped_list ~f:(fun ((_, typ), e) ->
                type_check_expression meta_ctx ctx e typ)))
        ~tag:
          "TypeInferenceError: Type mismatch between context and expressions \
           provided to substitute in."
      >>= fun () -> (*3- now context match*) Ok box_typ

let process_top_level meta_ctx ctx = function
  | Ast.TopLevelDefn.Definition (iddef, e) ->
      let id, typ = iddef in
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx ctx e typ >>= fun _ ->
      Ok (Ast.TypedTopLevelDefn.Definition (typ, iddef, e), meta_ctx, new_ctx)
  | Ast.TopLevelDefn.RecursiveDefinition (iddef, e) ->
      let id, typ = iddef in
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx new_ctx e typ >>= fun _ ->
      (*Note that here we type check in the new context (self reference)*)
      Ok
        ( Ast.TypedTopLevelDefn.RecursiveDefinition (typ, iddef, e),
          meta_ctx,
          new_ctx )
  | Ast.TopLevelDefn.Directive d ->
      let new_ctx =
        if Ast.Directive.equal d Ast.Directive.Reset then
          Typing_context.ObjTypingContext.create_empty_context ()
        else ctx
      in
      Ok (Ast.TypedTopLevelDefn.Directive d, meta_ctx, new_ctx)
  | Ast.TopLevelDefn.Expression e ->
      let open Or_error.Monad_infix in
      type_inference_expression meta_ctx ctx e >>= fun typ ->
      Ok (Ast.TypedTopLevelDefn.Expression (typ, e), meta_ctx, ctx)

let rec type_check_program_aux meta_ctx ctx program =
  match program with
  | [] -> Ok []
  | top :: tops ->
      let open Or_error.Monad_infix in
      process_top_level meta_ctx ctx top
      >>= fun (typed_top, new_meta, new_ctx) ->
      type_check_program_aux new_meta new_ctx tops >>= fun program_rest ->
      Ok (typed_top :: program_rest)

let type_check_program
    ?(meta_ctx = Typing_context.MetaTypingContext.create_empty_context ())
    ?(obj_ctx = Typing_context.ObjTypingContext.create_empty_context ()) program
    =
  let open Or_error.Monad_infix in
  program |> type_check_program_aux meta_ctx obj_ctx >>= fun typed_program ->
  Ast.TypedProgram.populate_index typed_program
