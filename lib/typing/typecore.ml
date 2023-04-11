open Core
open Lys_ast
open Lys_substitutions
open Lys_utils
(*
  TODO: Maybe instead of doing 1 pass, do 2 passes to distinguish Past->Ast and Type checking AST.   
*)

(* meta_ctx->ctx->Aast.Expr.t -> Ast.Typ.t -> unit Or_error.t *)

(* Check whether the type contains a function type *)
module TypSet = Set.Make (Ast.TypeIdentifier)

let rec includes_function_type_aux
    ~(type_ctx : Typing_context.TypeConstrContext.t)
    ~(types_visited : TypSet.t) = function
  | Ast.Typ.TFun (_, _) -> (true, types_visited)
  | Ast.Typ.TProd tlist ->
      List.fold ~init:(false, types_visited) tlist
        ~f:(fun (prev_result, types_visited) typ ->
          if prev_result then (prev_result, types_visited)
          else includes_function_type_aux typ ~type_ctx ~types_visited)
  | Ast.Typ.TSum (t1, t2) ->
      let outcome1, types_visited1 =
        includes_function_type_aux ~type_ctx ~types_visited t1
      in
      if outcome1 then (outcome1, types_visited1)
      else
        (includes_function_type_aux ~type_ctx ~types_visited:types_visited1) t2
  | Ast.Typ.TIdentifier (tlist, tid) -> (
      if TypSet.mem types_visited tid then (false, types_visited)
      else
        let types_visited = TypSet.add types_visited tid in
        let constr_records_opt =
          Typing_context.TypeConstrContext.get_constr_from_typ type_ctx
            tid
        in
        match constr_records_opt with
        | None -> (false, types_visited)
        | Some constr_records ->
            List.map ~f:(fun { arg_type; _ } -> arg_type) constr_records
            (* Coarse check for tlist: we aren't doing the type sub, but rather
               are checking whether there are functional types in tlist and in the list
               of construtors separately.
               Append THIS to tlist because we're making exactly the same check *)
            |> List.append (List.map ~f:(fun typ -> Some typ) tlist)
            |> List.fold ~init:(false, types_visited)
                 ~f:(fun (prev_result, types_visited) typ ->
                   if prev_result then (prev_result, types_visited)
                   else
                     match typ with
                     | None -> (false, types_visited)
                     | Some typ ->
                         includes_function_type_aux ~type_ctx ~types_visited typ)
      )
  | _ -> (false, types_visited)

let includes_function_type
    ~(type_ctx : Typing_context.TypeConstrContext.t)
    ?(types_visited : TypSet.t = TypSet.empty) typ =
  let res, _ = includes_function_type_aux ~type_ctx ~types_visited typ in
  res

let rec is_valid_type type_ctx typevar_ctx typ =
  let open Or_error.Monad_infix in
  match typ with
  | Ast.Typ.TUnit -> Ok ()
  | Ast.Typ.TBool -> Ok ()
  | Ast.Typ.TInt -> Ok ()
  | Ast.Typ.TChar -> Ok ()
  | Ast.Typ.TString -> Ok ()
  | Ast.Typ.TIdentifier (tlist, id) -> (
      match
        Typing_context.TypeConstrContext.get_constr_from_typ type_ctx id
      with
      | None ->
          Or_error.error
            "TypeValidityCheckError: The given type is invalid: unbound type \
             identifier (tid)"
            id [%sexp_of: Ast.TypeIdentifier.t]
      | Some constr_records -> (
          match constr_records with
          | [] ->
              (* tlist has to be empty*)
              if List.is_empty tlist then Ok ()
              else
                error
                  "TypeValidityCheckError: the type doesn't have constructors, \
                   hence has to be non-polymorphic, yet applied it to types."
                  (Ast.Typ.TIdentifier (tlist, id))
                  [%sexp_of: Ast.Typ.t]
          | { type_params; _ } :: _ ->
              let type_param_length = List.length type_params in
              let tlist_length = List.length tlist in
              if type_param_length = tlist_length then Ok ()
              else
                Or_error.errorf
                  "TypeValidityCheckError: expected # of type params = %i; \
                   gave %i type params"
                  type_param_length tlist_length))
  | Ast.Typ.TFun (t1, t2) | Ast.Typ.TSum (t1, t2) ->
      is_valid_type type_ctx typevar_ctx t1 >>= fun () ->
      is_valid_type type_ctx typevar_ctx t2
  | Ast.Typ.TBox (tvctx, ctx, t) ->
      let new_typevar_ctx =
        Typing_context.PolyTypeVarContext.add_all_mappings typevar_ctx
          (List.map ~f:(fun v -> (v, ())) tvctx)
      in
      List.map
        ~f:(fun (_, typ) -> is_valid_type type_ctx new_typevar_ctx typ)
        ctx
      |> Or_error.combine_errors_unit
      >>= fun () -> is_valid_type type_ctx new_typevar_ctx t
  | Ast.Typ.TProd tlist ->
      List.map tlist ~f:(fun t -> is_valid_type type_ctx typevar_ctx t)
      |> Or_error.combine_errors_unit
  | Ast.Typ.TRef t | Ast.Typ.TArray t -> is_valid_type type_ctx typevar_ctx t
  | Ast.Typ.TVar id ->
      if Typing_context.PolyTypeVarContext.is_in_context typevar_ctx id then
        Ok ()
      else
        error
          "TypeValidityCheckError: The given type is invalid: unbound type var \
           '[v]"
          id [%sexp_of: Ast.TypeVar.t]
  | Ast.Typ.TForall (id, typ) ->
      let new_typevar_context =
        Typing_context.PolyTypeVarContext.add_mapping typevar_ctx id ()
      in
      is_valid_type type_ctx new_typevar_context typ

let rec is_valid_type_for_recursion typ =
  match typ with
  | Ast.Typ.TFun (_, _) -> Ok ()
  | Ast.Typ.TForall (_, typ) -> is_valid_type_for_recursion typ
  | _ ->
      error
        "RecursionTypeCheckError: let rec declaration must have function type \
         or be of polymorphic function type."
        () [%sexp_of: unit]

let rec type_check_expression meta_ctx ctx
    (type_ctx : Typing_context.TypeConstrContext.t)
    (typevar_ctx : unit Typing_context.PolyTypeVarContext.t) expr typ =
  let open Or_error.Monad_infix in
  is_valid_type type_ctx typevar_ctx typ >>= fun () ->
  type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr
  >>= fun inferred_typ ->
  (*
     TODO: Dodgy equality check here: what if the DB indices aren't the same?
  *)
  if Ast.Typ.equal typ inferred_typ then Ok ()
  else
    error
      (Printf.sprintf
         "TypeCheckError: Inferred type %s\n\
         \ Not Equal to checked type %s. (expr, inferred_typ, typ, ctx, \
          meta_ctx)"
         (Ast.Typ.show inferred_typ)
         (Ast.Typ.show typ))
      (expr, inferred_typ, typ, ctx, meta_ctx)
      [%sexp_of:
        Ast.Expr.t
        * Ast.Typ.t
        * Ast.Typ.t
        * Ast.Typ.t Typing_context.ObjTypingContext.t
        * (Ast.TypeVarContext.t * Ast.Context.t * Ast.Typ.t)
          Typing_context.MetaTypingContext.t]

and type_inference_expression meta_ctx ctx type_ctx typevar_ctx e =
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
      | Ast.Constant.Unit -> Ok Ast.Typ.TUnit
      | Ast.Constant.Character _ -> Ok Ast.Typ.TChar
      | Ast.Constant.String _ -> Ok Ast.Typ.TString
      | _ ->
          error
            "TypeInferenceError: Can't type check a reference or an array \
             constant"
            c [%sexp_of: Ast.Constant.t])
  | Ast.Expr.UnaryOp (op, expr) -> (
      match op with
      | Ast.UnaryOperator.NEG ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx expr
            Ast.Typ.TInt
          >>= fun () -> Ok Ast.Typ.TInt
      | Ast.UnaryOperator.NOT ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx expr
            Ast.Typ.TBool
          >>= fun () -> Ok Ast.Typ.TBool
      | Ast.UnaryOperator.DEREF -> (
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr
          >>= fun typ ->
          match typ with
          | Ast.Typ.TRef typ -> Ok typ
          | _ ->
              Or_error.error
                "TypeInferenceError: Dereferencing a non-reference value \
                 (expr, typ)"
                (expr, typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t])
      | Ast.UnaryOperator.ARRAY_LEN -> (
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr
          >>= function
          | Ast.Typ.TArray _ -> Ok Ast.Typ.TInt
          | typ ->
              Or_error.error
                "TypeInferenceError: Getting length of a non-array value \
                 (expr, typ)"
                (expr, typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t]))
  | Ast.Expr.BinaryOp (op, expr, expr2) -> (
      let check_both typ_to_check ok_typ =
        type_check_expression meta_ctx ctx type_ctx typevar_ctx expr
          typ_to_check
        >>= fun () ->
        type_check_expression meta_ctx ctx type_ctx typevar_ctx expr2
          typ_to_check
        >>= fun () -> Ok ok_typ
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
          (*First check if non functional type: only allow equality between functional types*)
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr
          >>= fun typ ->
          if includes_function_type ~type_ctx typ then
            Or_error.error
              "TypeInferenceError: Can't decide equality between types \
               containing functional types."
              typ [%sexp_of: Ast.Typ.t]
          else
            let or_error =
              type_check_expression meta_ctx ctx type_ctx typevar_ctx expr2 typ
            in
            Or_error.tag or_error
              ~tag:
                ("TypeInferenceError: Type mismatch: both sides of equality \
                  must have same type: " ^ Ast.Typ.show typ)
            >>= fun _ -> Ok Ast.Typ.TBool
      | Ast.BinaryOperator.NEQ ->
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr
          >>= fun typ ->
          let or_error =
            type_check_expression meta_ctx ctx type_ctx typevar_ctx expr2 typ
          in
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
      | Ast.BinaryOperator.OR -> check_both Ast.Typ.TBool Ast.Typ.TBool
      | Ast.BinaryOperator.CHARSTRINGCONCAT ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx expr
            Ast.Typ.TChar
          >>= fun () ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx expr2
            Ast.Typ.TString
          >>= fun () -> Ok Ast.Typ.TString
      | Ast.BinaryOperator.STRINGCONCAT ->
          check_both Ast.Typ.TString Ast.Typ.TString
      | Ast.BinaryOperator.SEQ ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx expr
            Ast.Typ.TUnit
          >>= fun () ->
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr2
      | Ast.BinaryOperator.ASSIGN -> (
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr
          >>= fun t1 ->
          match t1 with
          | Ast.Typ.TRef typ ->
              (* Now check typ of second value *)
              ( type_check_expression meta_ctx ctx type_ctx typevar_ctx expr2 typ
              >>= fun () -> Ok Ast.Typ.TUnit )
              |> fun or_error ->
              Or_error.tag_arg or_error
                "TypeInferenceError: Assignment type mismatch: expected type \
                 (1), got different type. (expected_type, expr)"
                (typ, expr2) [%sexp_of: Ast.Typ.t * Ast.Expr.t]
          | _ ->
              Or_error.error
                "TypeInferenceError: Assigning to non-reference value (expr, \
                 typ)"
                (expr, t1) [%sexp_of: Ast.Expr.t * Ast.Typ.t])
      | Ast.BinaryOperator.ARRAY_INDEX -> (
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr
          >>= function
          | Ast.Typ.TArray val_typ ->
              type_check_expression meta_ctx ctx type_ctx typevar_ctx expr2
                Ast.Typ.TInt
              |> Or_error.tag
                   ~tag:"TypeInferenceError: array index not of int type"
              >>= fun () -> Ok val_typ
          | _ ->
              Or_error.error
                "TypeInferenceError: Can't index on non-array values [arr]" expr
                [%sexp_of: Ast.Expr.t]))
  | Ast.Expr.Prod exprs ->
      List.map exprs
        ~f:(type_inference_expression meta_ctx ctx type_ctx typevar_ctx)
      |> Or_error.combine_errors
      >>= fun typs -> Ok (Ast.Typ.TProd typs)
  (* | Ast.Expr.Fst expr -> (
         type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr >>= fun typ ->
         match typ with
         | Ast.Typ.TProd (typ1, _) -> Ok typ1
         | _ ->
             (*ERROR*)
             error "TypeInferenceError: Argument of fst must be of product type."
               expr [%sexp_of: Ast.Expr.t])
     | Ast.Expr.Snd expr -> (
         type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr >>= fun typ ->
         match typ with
         | Ast.Typ.TProd (_, typ2) -> Ok typ2
         | _ ->
             (*ERROR*)
             error "TypeInferenceError: Argument of snd must be of product type."
               expr [%sexp_of: Ast.Expr.t]) *)
  | Ast.Expr.Nth (expr, i) -> (
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr
      >>= fun typ ->
      (match typ with
      | Ast.Typ.TProd typs -> Ok typs
      | _ ->
          (*ERROR*)
          error "TypeInferenceError: Argument of  _[i] must be of product type."
            expr [%sexp_of: Ast.Expr.t])
      >>= fun typs ->
      (*Check length*)
      match List.nth typs i with
      | None ->
          error
            (Printf.sprintf
               "TypeInferenceError: can't project outside of the range of the \
                tuple length [0; %s]"
               (Int.to_string (List.length typs)))
            (expr, i) [%sexp_of: Ast.Expr.t * int]
      | Some typ -> Ok typ)
  | Ast.Expr.Left (t1, t2, expr) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx expr t1)
        ~tag:
          ("TypeInferenceError: the given expression must be of the left type \
            of the given sum type: " ^ Ast.Typ.show t1)
      >>= fun () -> Ok (Ast.Typ.TSum (t1, t2))
  | Ast.Expr.Right (t1, t2, expr) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx expr t2)
        ~tag:
          ("TypeInferenceError: the given expression must be of of the right \
            type of the given sum type: " ^ Ast.Typ.show t2)
      >>= fun () -> Ok (Ast.Typ.TSum (t1, t2))
  | Ast.Expr.Case (e, iddef1, e1, iddef2, e2) ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx e
      >>= fun e_typ ->
      (match e_typ with
      | Ast.Typ.TSum (_, _) -> Ok ()
      | typ ->
          error "TypeInferenceError: argument of `match` must be of a sum type."
            typ [%sexp_of: Ast.Typ.t])
      >>= fun () ->
      let id1, typ1 = iddef1 in
      let new_ctx1 = Typing_context.ObjTypingContext.add_mapping ctx id1 typ1 in
      type_inference_expression meta_ctx new_ctx1 type_ctx typevar_ctx e1
      >>= fun e1_type ->
      let id2, typ2 = iddef2 in
      let new_ctx2 = Typing_context.ObjTypingContext.add_mapping ctx id2 typ2 in
      (*Types should match, or there is a Type mismatch error*)
      let or_error =
        type_check_expression meta_ctx new_ctx2 type_ctx typevar_ctx e2 e1_type
      in
      Or_error.tag or_error
        ~tag:
          "TypeInferenceError: Type mismatch: Each outcome of a match clause \
           must give the same type"
      >>= fun () -> Ok e1_type
  | Ast.Expr.Lambda ((id, typ), e) ->
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      type_inference_expression meta_ctx new_ctx type_ctx typevar_ctx e
      >>= fun e_typ -> Ok (Ast.Typ.TFun (typ, e_typ))
  | Ast.Expr.Application (e1, e2) ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx e1
      >>= fun e1_typ ->
      (match e1_typ with
      | Ast.Typ.TFun (arg_typ, res_typ) -> Ok (arg_typ, res_typ)
      | actual_typ ->
          error
            "TypeInferenceError: only functions can be applied; the expression \
             given is not a function."
            actual_typ [%sexp_of: Ast.Typ.t])
      >>= fun (arg_typ, res_typ) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx e2 arg_typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: Type mismatch: expected argument of type %s."
             (Ast.Typ.show arg_typ))
      >>= fun () -> Ok res_typ
  | Ast.Expr.IfThenElse (b, e1, e2) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx b Ast.Typ.TBool)
        ~tag:
          "TypeInferenceError: the predicate of an if-then-else expression \
           must be of boolean type"
      >>= fun () ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx e1
      >>= fun e1_typ ->
      Or_error.tag_arg
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx e2 e1_typ)
        "TypeInferenceError: All outcomes of an if-then-else expression must \
         be of the same type."
        (e2, e1_typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t]
      >>= fun () -> Ok e1_typ
  | Ast.Expr.LetBinding ((id, typ), e, e2) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx e typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: variable %s is declared of type %s but bound \
              to an expression of a different type"
             (Ast.ObjIdentifier.show id)
             (Ast.Typ.show typ))
      >>= fun () ->
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      type_inference_expression meta_ctx new_ctx type_ctx typevar_ctx e2
  | Ast.Expr.LetRec ((id, typ), e, e2) ->
      (*We only allow types which are of the form A->B*)
      is_valid_type_for_recursion typ >>= fun () ->
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      Or_error.tag
        (type_check_expression meta_ctx new_ctx type_ctx typevar_ctx e typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: recursive variable %s is declared of type %s \
              but bound to an expression of a different type"
             (Ast.ObjIdentifier.show id)
             (Ast.Typ.show typ))
      >>= fun () ->
      type_inference_expression meta_ctx new_ctx type_ctx typevar_ctx e2
  | Ast.Expr.LetRecMutual (idddef_e_list, e2) ->
      (* First get the types *)
      let iddefs, _ = List.unzip idddef_e_list in
      (* Then check the types are functional *)
      List.map iddefs ~f:(fun (_, typ) -> is_valid_type_for_recursion typ)
      |> Or_error.combine_errors_unit
      >>= fun () ->
      let new_ctx =
        Typing_context.ObjTypingContext.add_all_mappings ctx iddefs
      in
      List.map idddef_e_list ~f:(fun ((id, typ), e) ->
          type_check_expression meta_ctx new_ctx type_ctx typevar_ctx e typ
          |> Or_error.tag
               ~tag:
                 (Printf.sprintf
                    "TypeInferenceError: recursive variable %s is declared of \
                     type %s but bound to an expression of a different type"
                    (Ast.ObjIdentifier.show id)
                    (Ast.Typ.show typ)))
      |> Or_error.combine_errors_unit
      |> Or_error.tag
           ~tag:
             "TypeInferenceError: Type error at mutually recursive declaration"
      >>= fun () ->
      type_inference_expression meta_ctx new_ctx type_ctx typevar_ctx e2
  | Ast.Expr.Box (box_tvctx, box_context, e) ->
      (if Ast.Context.contains_duplicate_ids box_context then
       error
         "TypeInferenceError: there are duplicates in the provided box context"
         box_context [%sexp_of: Ast.Context.t]
      else if Ast.TypeVarContext.contains_duplicates box_tvctx then
        error
          "TypeInferenceError: there are duplicates in the provided type var \
           context"
          box_tvctx [%sexp_of: Ast.TypeVarContext.t]
      else Ok ())
      >>= fun () ->
      let new_typevar_ctx =
        Typing_context.PolyTypeVarContext.add_all_mappings typevar_ctx
          (List.map box_tvctx ~f:(fun v -> (v, ())))
      in
      let new_ctx =
        Typing_context.ObjTypingContext.add_all_mappings
          (Typing_context.ObjTypingContext.create_empty_context ())
          box_context (*Bug here*)
      in
      type_inference_expression meta_ctx new_ctx type_ctx new_typevar_ctx e
      >>= fun e_typ -> Ok (Ast.Typ.TBox (box_tvctx, box_context, e_typ))
  | Ast.Expr.LetBox (metaid, e, e2) ->
      ( (*First check that e gives a box*)
        type_inference_expression meta_ctx ctx type_ctx typevar_ctx e
      >>= fun e_typ ->
        match e_typ with
        | Ast.Typ.TBox (box_tvctx, box_context, box_typ) ->
            Ok (box_tvctx, box_context, box_typ)
        | _ ->
            Or_error.error "TypeInferenceError: Can only unbox a boxed term."
              (metaid, e, e_typ)
              [%sexp_of: Ast.MetaIdentifier.t * Ast.Expr.t * Ast.Typ.t] )
      >>= fun (box_tvctx, box_context, box_typ) ->
      (*Add it to the meta context and continue*)
      let new_meta_ctx =
        Typing_context.MetaTypingContext.add_mapping meta_ctx metaid
          (box_tvctx, box_context, box_typ)
      in
      type_inference_expression new_meta_ctx ctx type_ctx typevar_ctx e2
  | Ast.Expr.Closure (meta_id, typs, exprs) ->
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
              (Ast.TypeVarContext.t * Ast.Context.t * Ast.Typ.t)
              Typing_context.MetaTypingContext.t
              * Ast.MetaIdentifier.t]
      | Some (box_tvctx, box_context, box_typ) ->
          Ok (box_tvctx, box_context, box_typ))
      >>= fun (box_tvctx, box_context, box_typ) ->
      (*I- Check typs match box_tvctx *)
      if List.length box_tvctx <> List.length typs then
        Or_error.error
          "TypeInferenceError: type list provided unequal to the box's type \
           var context length (closure_expr)"
          (Ast.Expr.Closure (meta_id, typs, exprs))
          [%sexp_of: Ast.Expr.t]
      else
        (* II- Check expressions match box_context *)
        (* 1- check length *)
        let zipped_list_option = List.zip box_context exprs in
        (match zipped_list_option with
        | List.Or_unequal_lengths.Ok zipped_list -> Ok zipped_list
        | List.Or_unequal_lengths.Unequal_lengths ->
            error
              "TypeInferenceError: the number of arguments to the `with` \
               expression does not match the size of the context. \
               (box_context, closure_expr)"
              (box_context, Ast.Expr.Closure (meta_id, typs, exprs))
              [%sexp_of: Ast.Context.t * Ast.Expr.t])
        >>= fun zipped_list ->
        (* 2- check types *)
        Or_error.tag
          (Or_error.combine_errors_unit
             (List.map zipped_list ~f:(fun ((_, typ), e) ->
                  (* Subsitute type *)
                  Substitutions.sim_type_type_substitute typs box_tvctx typ
                  >>= fun typ ->
                  type_check_expression meta_ctx ctx type_ctx typevar_ctx e typ)))
          ~tag:
            "TypeInferenceError: Type mismatch between context and expressions \
             provided to substitute in."
        >>= fun () ->
        (* Now substitute the typ *)
        Substitutions.sim_type_type_substitute typs box_tvctx box_typ
  | Ast.Expr.Match (e, pattn_expr_list) -> (
      (*
      1- infer type of e
      2- Cases: 
      - e is a product: check if all patterns are of the correct type
      - e is a sum: check if all the patterns are of the correct type
      - e is a datatype:  check if all the patterns are of the correct type
      and give the zipped list of binder-type
      3- check that if we put all the binders in the types required we get what we want
    *)
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx e
      >>= fun inferred_typ ->
      (match inferred_typ with
      | Ast.Typ.TProd typs ->
          List.map pattn_expr_list ~f:(fun (pattn, expr) ->
              (*1- Check the pattern is a correct one*)
              match pattn with
              | Ast.Pattern.Prod ids ->
                  (*2- Now match the binders*)
                  Utils.try_zip_list_or_error ids typs
                    (error
                       "TypeInferenceError: pattern argument # doesn't \
                        correspond to the expected #"
                       (pattn, inferred_typ)
                       [%sexp_of: Ast.Pattern.t * Ast.Typ.t])
                  >>= fun zipped_list ->
                  (*3- Infer type*)
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_all_mappings ctx
                      zipped_list
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx expr
              | Ast.Pattern.Id id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      inferred_typ
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    expr
              | _ ->
                  Or_error.error
                    "TypeInferenceError: Mismatch between type and pattern."
                    (pattn, inferred_typ) [%sexp_of: Ast.Pattern.t * Ast.Typ.t])
          |> Or_error.combine_errors
          |> Or_error.tag ~tag:"TypeInferenceError: on `match` clause."
      | Ast.Typ.TSum (t1, t2) ->
          List.map pattn_expr_list ~f:(fun (pattn, expr) ->
              match pattn with
              | Ast.Pattern.Inl id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id t1
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx expr
              | Ast.Pattern.Inr id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id t2
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx expr
              | Ast.Pattern.Id id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      inferred_typ
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    expr
              | _ ->
                  Or_error.error
                    "TypeInferenceError: Mismatch between type and pattern."
                    (pattn, inferred_typ) [%sexp_of: Ast.Pattern.t * Ast.Typ.t])
          |> Or_error.combine_errors
      | Ast.Typ.TIdentifier (tlist, tid) ->
          (* Check that each thing is a constructor of the right type. *)
          List.map pattn_expr_list ~f:(fun (pattn, expr) ->
              match pattn with
              | Ast.Pattern.Id id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      inferred_typ
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    expr
              | Ast.Pattern.Datatype (constr, id_list) ->
                  (*1- check that constructor is of the type needed*)
                  let constr_record_opt =
                    Typing_context.TypeConstrContext.get_typ_from_constr
                      type_ctx constr
                  in
                  (match constr_record_opt with
                  | None ->
                      Or_error.error
                        "TypeInferenceError: Constructor doesn't exist" constr
                        [%sexp_of: Ast.Constructor.t]
                  | Some constr_record ->
                      if
                        Ast.TypeIdentifier.equal tid
                          constr_record.belongs_to_typ
                      then Ok constr_record
                      else
                        Or_error.error
                          "TypeInferenceError: Constructor doesn't match type"
                          (Ast.Pattern.Datatype (constr, id_list), tid)
                          [%sexp_of: Ast.Pattern.t * Ast.TypeIdentifier.t])
                  >>= fun constr_record ->
                  (*2- get list*)
                  let typs =
                    match constr_record.arg_type with
                    | Some (Ast.Typ.TProd typs) -> typs
                    | Some typ -> [ typ ]
                    | None -> []
                  in
                  (* 2.5- *NEW*: Do the type substitutions *)
                  List.map
                    ~f:
                      (Substitutions.sim_type_type_substitute tlist
                         constr_record.type_params)
                    typs
                  |> Or_error.combine_errors
                  |> Or_error.tag
                       ~tag:
                         "TypeInferenceError: Error when type substituting the \
                          type arguments in the constructor param types."
                  >>= fun typs ->
                  (*3- Match id list*)
                  Utils.try_zip_list_or_error id_list typs
                    (Or_error.error
                       (Printf.sprintf
                          "TypeInferenceError: Expected # of arguments: %d, \
                           actual number of arguments: %d"
                          (List.length typs) (List.length id_list))
                       (id_list, typs)
                       [%sexp_of: Ast.ObjIdentifier.t list * Ast.Typ.t list])
                  >>= fun zipped_list ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_all_mappings ctx
                      zipped_list
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx expr
              | _ ->
                  Or_error.error
                    "TypeInferenceError: Mismatch between type and pattern."
                    (pattn, inferred_typ) [%sexp_of: Ast.Pattern.t * Ast.Typ.t])
          |> Or_error.combine_errors
      | Ast.Typ.TString ->
          List.map pattn_expr_list ~f:(fun (pattn, expr) ->
              match pattn with
              | Ast.Pattern.Id id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      inferred_typ
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    expr
              | Ast.Pattern.String _ ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    expr
              | Ast.Pattern.ConcatCharString (cid, sid) ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_all_mappings ctx
                      [ (cid, Ast.Typ.TChar); (sid, Ast.Typ.TString) ]
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx expr
              | _ ->
                  Or_error.error
                    "TypeInferenceError: Mismatch between type and pattern."
                    (pattn, inferred_typ) [%sexp_of: Ast.Pattern.t * Ast.Typ.t])
          |> Or_error.combine_errors
      | _ ->
          Or_error.error
            "TypeInferenceError: Match clause only supports product types, sum \
             types and identifier types"
            inferred_typ [%sexp_of: Ast.Typ.t])
      >>= fun inferred_types_for_each_outcome ->
      (* Finally, check that all branches have the same type *)
      match
        List.all_equal inferred_types_for_each_outcome ~equal:Ast.Typ.equal
      with
      | None ->
          error
            "TypeInferenceError: all outcomes of a Match expr must have the \
             same type"
            (List.dedup_and_sort inferred_types_for_each_outcome
               ~compare:Ast.Typ.compare)
            [%sexp_of: Ast.Typ.t list]
      | Some typ -> Ok typ)
  | Ast.Expr.Constr (constr, tlist, e_opt) -> (
      (* Check if constructor is defined *)
      match
        Typing_context.TypeConstrContext.get_typ_from_constr type_ctx
          constr
      with
      | None ->
          Or_error.error "TypeInferenceError: Constructor is undefined" constr
            [%sexp_of: Ast.Constructor.t]
      | Some constr_record ->
          (match (constr_record.arg_type, e_opt) with
          | None, None -> Ok ()
          | Some t, Some e ->
              (* *NEW* Check that (poly) type is valid *)
              Substitutions.sim_type_type_substitute tlist
                constr_record.type_params t
              >>= fun t ->
              (* Defined, so check arguments *)
              type_check_expression meta_ctx ctx type_ctx typevar_ctx e t
              |> Or_error.tag
                   ~tag:
                     (Printf.sprintf
                        "TypeInferenceError: On Constructor %s: argument type \
                         mismatch. Expected type %s"
                        (Ast.Constructor.get_name constr)
                        (Ast.Typ.show t))
          | _ ->
              Or_error.error
                (Printf.sprintf
                   "TypeInferenceError: On Constructor %s, argument type \
                    mismatch."
                   (Ast.Constructor.show constr))
                (constr_record.arg_type, e_opt)
                [%sexp_of: Ast.Typ.t option * Ast.Expr.t option])
          >>= fun _ ->
          Ok (Ast.Typ.TIdentifier (tlist, constr_record.belongs_to_typ)))
  | Ast.Expr.Lift (typ, expr) ->
      (* Only support lifting primitive types
         (int, unit, char...), box types and all other thing without function types *)
      (* check if typ exists has function types *)
      if includes_function_type ~type_ctx typ then
        Or_error.error
          "TypeInferenceError: Type given as argument to `lift` cannot contain \
           function types. Given type: (typ)"
          typ [%sexp_of: Ast.Typ.t]
      else
        type_check_expression meta_ctx ctx type_ctx typevar_ctx expr typ
        >>= fun () -> Ok (Ast.Typ.TBox ([], [], typ))
  | Ast.Expr.EValue v ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        (Ast.Value.to_expr v)
  | Ast.Expr.Ref expr ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx expr
      >>= fun typ -> Ok (Ast.Typ.TRef typ)
  | Ast.Expr.While (p, e) ->
      type_check_expression meta_ctx ctx type_ctx typevar_ctx p Ast.Typ.TBool
      |> Or_error.tag
           ~tag:
             "TypeInferenceError: Expected bool type for while loop predicate."
      >>= fun () ->
      type_check_expression meta_ctx ctx type_ctx typevar_ctx e Ast.Typ.TUnit
      |> Or_error.tag
           ~tag:"TypeInferenceError: Expected unit type body for while loops"
      >>= fun () -> Ok Ast.Typ.TUnit
  | Ast.Expr.Array es -> (
      match es with
      | [] ->
          Or_error.error "TypeInferenceError: Array is empty"
            (Ast.Expr.Array es) [%sexp_of: Ast.Expr.t]
      | x :: xs ->
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx x
          >>= fun x_typ ->
          (* Check that all other ones are of the same type *)
          List.map xs ~f:(fun x ->
              type_check_expression meta_ctx ctx type_ctx typevar_ctx x x_typ)
          |> Or_error.combine_errors_unit
          >>= fun () -> Ok (Ast.Typ.TArray x_typ))
  | Ast.Expr.ArrayAssign (arr, index, e) -> (
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx arr
      >>= function
      | Ast.Typ.TArray val_typ ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx index
            Ast.Typ.TInt
          |> fun or_error ->
          Or_error.tag_arg or_error
            "TypeInferenceError: At Array Assignment: index not of type int \
             [index]"
            index [%sexp_of: Ast.Expr.t]
          >>= fun () ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx e val_typ
          |> fun or_error ->
          Or_error.tag_arg or_error
            "TypeInferenceError: At Array Assignment: type mismatch between \
             expression and array [arr, expression]"
            (arr, e) [%sexp_of: Ast.Expr.t * Ast.Expr.t]
          >>= fun () -> Ok Ast.Typ.TUnit
      | _ ->
          Or_error.error
            "TypeInferenceError: At Array Assignment: arr not of array type \
             [arr]"
            arr [%sexp_of: Ast.Expr.t])
  | Ast.Expr.BigLambda (v, e) ->
      let new_typevar_ctx =
        Typing_context.PolyTypeVarContext.add_mapping typevar_ctx v ()
      in
      type_inference_expression meta_ctx ctx type_ctx new_typevar_ctx e
      >>= fun typ -> Ok (Ast.Typ.TForall (v, typ))
  | Ast.Expr.TypeApply (e, t) -> (
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx e
      >>= fun e_typ ->
      (* Check is right type (big lambda) *)
      match e_typ with
      | Ast.Typ.TForall (v, typ) ->
          (* Check type validity *)
          is_valid_type type_ctx typevar_ctx t >>= fun () ->
          Substitutions.type_type_substitute t v typ
      | _ ->
          error
            "TypeInferenceError: At type apply, the left operand must be a big \
             lambda term. (left, right)"
            (e, t) [%sexp_of: Ast.Expr.t * Ast.Typ.t])

let process_top_level meta_ctx ctx type_ctx typevar_ctx = function
  | Ast.TopLevelDefn.Definition (iddef, e) ->
      let id, typ = iddef in
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx ctx type_ctx typevar_ctx e typ >>= fun _ ->
      Ok
        ( Ast.TypedTopLevelDefn.Definition (typ, iddef, e),
          meta_ctx,
          new_ctx,
          type_ctx,
          typevar_ctx )
  | Ast.TopLevelDefn.RecursiveDefinition (iddef, e) ->
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx ctx type_ctx typevar_ctx
        (Ast.Expr.LetRec (iddef, e, Ast.Expr.Constant Ast.Constant.Unit))
        Ast.Typ.TUnit
      >>= fun _ ->
      (*Note that here we type check in the new context (self reference)*)
      let id, typ = iddef in
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      Ok
        ( Ast.TypedTopLevelDefn.RecursiveDefinition (typ, iddef, e),
          meta_ctx,
          new_ctx,
          type_ctx,
          typevar_ctx )
  | Ast.TopLevelDefn.MutualRecursiveDefinition iddef_e_list ->
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx ctx type_ctx typevar_ctx
        (Ast.Expr.LetRecMutual
           (iddef_e_list, Ast.Expr.Constant Ast.Constant.Unit))
        Ast.Typ.TUnit
      |> Or_error.tag
           ~tag:
             (Printf.sprintf
                "TypeCheckError: Type Mismatch at Mutual recursive definition \
                 of variables [%s]"
                (List.fold_left iddef_e_list ~init:"" ~f:(fun acc (iddef, _) ->
                     acc ^ Ast.IdentifierDefn.show iddef ^ "; ")))
      >>= fun () ->
      let new_ctx =
        Typing_context.ObjTypingContext.add_all_mappings ctx
          (List.map iddef_e_list ~f:(fun (iddef, _) -> iddef))
      in
      Ok
        ( Ast.TypedTopLevelDefn.MutualRecursiveDefinition
            (List.map iddef_e_list ~f:(fun ((id, typ), e) ->
                 (typ, (id, typ), e))),
          meta_ctx,
          new_ctx,
          type_ctx,
          typevar_ctx )
  | Ast.TopLevelDefn.Directive d ->
      let new_ctx =
        if Ast.Directive.equal d Ast.Directive.Reset then
          Typing_context.ObjTypingContext.create_empty_context ()
        else ctx
      in
      Ok
        ( Ast.TypedTopLevelDefn.Directive d,
          meta_ctx,
          new_ctx,
          type_ctx,
          typevar_ctx )
  | Ast.TopLevelDefn.Expression e ->
      let open Or_error.Monad_infix in
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx e
      >>= fun typ ->
      Ok
        ( Ast.TypedTopLevelDefn.Expression (typ, e),
          meta_ctx,
          ctx,
          type_ctx,
          typevar_ctx )
  | Ast.TopLevelDefn.DatatypeDecl id_constr_typ_list_list ->
      let open Or_error.Monad_infix in
      List.fold id_constr_typ_list_list ~init:(Ok type_ctx)
        ~f:(fun acc (tvctx, tid, constr_typ_list) ->
          acc >>= fun running_type_ctx ->
          Typing_context.TypeConstrContext.add_typ_from_decl
            running_type_ctx
            (tvctx, tid, constr_typ_list))
      >>= fun new_type_ctx ->
      Ok
        ( Ast.TypedTopLevelDefn.DatatypeDecl id_constr_typ_list_list,
          meta_ctx,
          ctx,
          new_type_ctx,
          typevar_ctx )

let rec type_check_program_aux meta_ctx ctx type_ctx typevar_ctx program =
  match program with
  | [] -> Ok []
  | top :: tops ->
      let open Or_error.Monad_infix in
      process_top_level meta_ctx ctx type_ctx typevar_ctx top
      >>= fun (typed_top, new_meta, new_ctx, new_type_ctx, new_typevar_ctx) ->
      type_check_program_aux new_meta new_ctx new_type_ctx new_typevar_ctx tops
      >>= fun program_rest -> Ok (typed_top :: program_rest)

let type_check_program
    ?(meta_ctx = Typing_context.MetaTypingContext.create_empty_context ())
    ?(obj_ctx = Typing_context.ObjTypingContext.create_empty_context ())
    ?(type_ctx = Typing_context.TypeConstrContext.empty)
    ?(typevar_ctx = Typing_context.PolyTypeVarContext.create_empty_context ())
    program =
  program |> type_check_program_aux meta_ctx obj_ctx type_ctx typevar_ctx
