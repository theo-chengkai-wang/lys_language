open Core
open Lys_ast
open Lys_utils
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

(* Check whether the type contains a function type *)
module TypSet = Set.Make (Ast.TypeIdentifier)

let rec includes_function_type_aux
    ~(type_ctx : Typing_context.TypeConstrTypingContext.t)
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
  | Ast.Typ.TIdentifier tid -> (
      if TypSet.mem types_visited tid then (false, types_visited)
      else
        let types_visited = TypSet.add types_visited tid in
        let constr_records_opt =
          Typing_context.TypeConstrTypingContext.get_constr_from_typ type_ctx
            tid
        in
        match constr_records_opt with
        | None -> (false, types_visited)
        | Some constr_records ->
            List.fold constr_records ~init:(false, types_visited)
              ~f:(fun (prev_result, types_visited) record ->
                if prev_result then (prev_result, types_visited)
                else
                  match record.arg_type with
                  | None -> (false, types_visited)
                  | Some typ ->
                      includes_function_type_aux ~type_ctx ~types_visited typ))
  | _ -> (false, types_visited)

let includes_function_type
    ~(type_ctx : Typing_context.TypeConstrTypingContext.t)
    ?(types_visited : TypSet.t = TypSet.empty) typ =
  let res, _ = includes_function_type_aux ~type_ctx ~types_visited typ in
  res

let rec type_check_expression meta_ctx ctx
    (type_ctx : Typing_context.TypeConstrTypingContext.t) expr typ =
  let open Or_error.Monad_infix in
  type_inference_expression meta_ctx ctx type_ctx expr >>= fun inferred_typ ->
  if Ast.Typ.equal typ inferred_typ then Ok ()
  else
    error
      (Printf.sprintf
         "TypeCheckError: Inferred type %s\n Not Equal to checked type %s."
         (Ast.Typ.show inferred_typ)
         (Ast.Typ.show typ))
      (expr, inferred_typ, typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t * Ast.Typ.t]

and type_inference_expression meta_ctx ctx type_ctx e =
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
          type_check_expression meta_ctx ctx type_ctx expr Ast.Typ.TInt
          >>= fun () -> Ok Ast.Typ.TInt
      | Ast.UnaryOperator.NOT ->
          type_check_expression meta_ctx ctx type_ctx expr Ast.Typ.TBool
          >>= fun () -> Ok Ast.Typ.TBool)
  | Ast.Expr.BinaryOp (op, expr, expr2) -> (
      let check_both typ_to_check ok_typ =
        type_check_expression meta_ctx ctx type_ctx expr typ_to_check
        >>= fun () ->
        type_check_expression meta_ctx ctx type_ctx expr2 typ_to_check
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
          type_inference_expression meta_ctx ctx type_ctx expr >>= fun typ ->
          let or_error =
            type_check_expression meta_ctx ctx type_ctx expr2 typ
          in
          Or_error.tag or_error
            ~tag:
              ("TypeInferenceError: Type mismatch: both sides of equality must \
                have same type: " ^ Ast.Typ.show typ)
          >>= fun _ -> Ok Ast.Typ.TBool
      | Ast.BinaryOperator.NEQ ->
          type_inference_expression meta_ctx ctx type_ctx expr >>= fun typ ->
          let or_error =
            type_check_expression meta_ctx ctx type_ctx expr2 typ
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
      | Ast.BinaryOperator.OR -> check_both Ast.Typ.TBool Ast.Typ.TBool)
  | Ast.Expr.Prod exprs ->
      List.map exprs ~f:(type_inference_expression meta_ctx ctx type_ctx)
      |> Or_error.combine_errors
      >>= fun typs -> Ok (Ast.Typ.TProd typs)
  (* | Ast.Expr.Fst expr -> (
         type_inference_expression meta_ctx ctx type_ctx expr >>= fun typ ->
         match typ with
         | Ast.Typ.TProd (typ1, _) -> Ok typ1
         | _ ->
             (*ERROR*)
             error "TypeInferenceError: Argument of fst must be of product type."
               expr [%sexp_of: Ast.Expr.t])
     | Ast.Expr.Snd expr -> (
         type_inference_expression meta_ctx ctx type_ctx expr >>= fun typ ->
         match typ with
         | Ast.Typ.TProd (_, typ2) -> Ok typ2
         | _ ->
             (*ERROR*)
             error "TypeInferenceError: Argument of snd must be of product type."
               expr [%sexp_of: Ast.Expr.t]) *)
  | Ast.Expr.Nth (expr, i) -> (
      type_inference_expression meta_ctx ctx type_ctx expr >>= fun typ ->
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
        (type_check_expression meta_ctx ctx type_ctx expr t1)
        ~tag:
          ("TypeInferenceError: the given expression must be of the left type \
            of the given sum type: " ^ Ast.Typ.show t1)
      >>= fun () -> Ok (Ast.Typ.TSum (t1, t2))
  | Ast.Expr.Right (t1, t2, expr) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx expr t2)
        ~tag:
          ("TypeInferenceError: the given expression must be of of the right \
            type of the given sum type: " ^ Ast.Typ.show t2)
      >>= fun () -> Ok (Ast.Typ.TSum (t1, t2))
  | Ast.Expr.Case (e, iddef1, e1, iddef2, e2) ->
      type_inference_expression meta_ctx ctx type_ctx e >>= fun e_typ ->
      (match e_typ with
      | Ast.Typ.TSum (_, _) -> Ok ()
      | typ ->
          error "TypeInferenceError: argument of `match` must be of a sum type."
            typ [%sexp_of: Ast.Typ.t])
      >>= fun () ->
      let id1, typ1 = iddef1 in
      let new_ctx1 = Typing_context.ObjTypingContext.add_mapping ctx id1 typ1 in
      type_inference_expression meta_ctx new_ctx1 type_ctx e1 >>= fun e1_type ->
      let id2, typ2 = iddef2 in
      let new_ctx2 = Typing_context.ObjTypingContext.add_mapping ctx id2 typ2 in
      (*Types should match, or there is a Type mismatch error*)
      let or_error =
        type_check_expression meta_ctx new_ctx2 type_ctx e2 e1_type
      in
      Or_error.tag or_error
        ~tag:
          "TypeInferenceError: Type mismatch: Each outcome of a match clause \
           must give the same type"
      >>= fun () -> Ok e1_type
  | Ast.Expr.Lambda ((id, typ), e) ->
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      type_inference_expression meta_ctx new_ctx type_ctx e >>= fun e_typ ->
      Ok (Ast.Typ.TFun (typ, e_typ))
  | Ast.Expr.Application (e1, e2) ->
      type_inference_expression meta_ctx ctx type_ctx e1 >>= fun e1_typ ->
      (match e1_typ with
      | Ast.Typ.TFun (arg_typ, res_typ) -> Ok (arg_typ, res_typ)
      | actual_typ ->
          error
            "TypeInferenceError: only functions can be applied; the expression \
             given is not a function."
            actual_typ [%sexp_of: Ast.Typ.t])
      >>= fun (arg_typ, res_typ) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx e2 arg_typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: Type mismatch: expected argument of type %s."
             (Ast.Typ.show arg_typ))
      >>= fun () -> Ok res_typ
  | Ast.Expr.IfThenElse (b, e1, e2) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx b Ast.Typ.TBool)
        ~tag:
          "TypeInferenceError: the predicate of an if-then-else expression \
           must be of boolean type"
      >>= fun () ->
      type_inference_expression meta_ctx ctx type_ctx e1 >>= fun e1_typ ->
      Or_error.tag_arg
        (type_check_expression meta_ctx ctx type_ctx e2 e1_typ)
        "TypeInferenceError: All outcomes of an if-then-else expression must \
         be of the same type."
        (e2, e1_typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t]
      >>= fun () -> Ok e1_typ
  | Ast.Expr.LetBinding ((id, typ), e, e2) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx e typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: variable %s is declared of type %s but bound \
              to an expression of a different type"
             (Ast.ObjIdentifier.show id)
             (Ast.Typ.show typ))
      >>= fun () ->
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      type_inference_expression meta_ctx new_ctx type_ctx e2
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
        (type_check_expression meta_ctx new_ctx type_ctx e typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: recursive variable %s is declared of type %s \
              but bound to an expression of a different type"
             (Ast.ObjIdentifier.show id)
             (Ast.Typ.show typ))
      >>= fun () -> type_inference_expression meta_ctx new_ctx type_ctx e2
  | Ast.Expr.Box (context, e) ->
      (if box_context_contains_duplicates context then
       error
         "TypeInferenceError: there are duplicates in the provided box context"
         context [%sexp_of: Ast.Context.t]
      else Ok ())
      >>= fun () ->
      let new_ctx =
        Typing_context.ObjTypingContext.add_all_mappings
          (Typing_context.ObjTypingContext.create_empty_context ())
          context (*Bug here*)
      in
      type_inference_expression meta_ctx new_ctx type_ctx e >>= fun e_typ ->
      Ok (Ast.Typ.TBox (context, e_typ))
  | Ast.Expr.LetBox (metaid, e, e2) ->
      ( (*First check that e gives a box*)
        type_inference_expression meta_ctx ctx type_ctx e
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
      type_inference_expression new_meta_ctx ctx type_ctx e2
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
                type_check_expression meta_ctx ctx type_ctx e typ)))
        ~tag:
          "TypeInferenceError: Type mismatch between context and expressions \
           provided to substitute in."
      >>= fun () -> (*3- now context match*) Ok box_typ
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
      type_inference_expression meta_ctx ctx type_ctx e
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
                  type_inference_expression meta_ctx new_ctx type_ctx expr
              | Ast.Pattern.Id id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      inferred_typ
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx expr
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
                  type_inference_expression meta_ctx new_ctx type_ctx expr
              | Ast.Pattern.Inr id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id t2
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx expr
              | Ast.Pattern.Id id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      inferred_typ
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx expr
              | _ ->
                  Or_error.error
                    "TypeInferenceError: Mismatch between type and pattern."
                    (pattn, inferred_typ) [%sexp_of: Ast.Pattern.t * Ast.Typ.t])
          |> Or_error.combine_errors
      | Ast.Typ.TIdentifier tid ->
          (* Check that each thing is a constructor of the right type. *)
          List.map pattn_expr_list ~f:(fun (pattn, expr) ->
              match pattn with
              | Ast.Pattern.Id id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      inferred_typ
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx expr
              | Ast.Pattern.Datatype (constr, id_list) ->
                  (*1- check that constructor is of the type needed*)
                  let constr_record_opt =
                    Typing_context.TypeConstrTypingContext.get_typ_from_constr
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
                          (constr, tid)
                          [%sexp_of: Ast.Constructor.t * Ast.TypeIdentifier.t])
                  >>= fun constr_record ->
                  (*2- get list*)
                  let typs =
                    match constr_record.arg_type with
                    | Some (Ast.Typ.TProd typs) -> typs
                    | Some typ -> [ typ ]
                    | None -> []
                  in
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
                  type_inference_expression meta_ctx new_ctx type_ctx expr
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
  | Ast.Expr.Constr (constr, e_opt) -> (
      (* Check if constructor is defined *)
      match
        Typing_context.TypeConstrTypingContext.get_typ_from_constr type_ctx
          constr
      with
      | None ->
          Or_error.error "TypeInferenceError: Constructor is undefined" constr
            [%sexp_of: Ast.Constructor.t]
      | Some constr_record ->
          (match (constr_record.arg_type, e_opt) with
          | None, None -> Ok ()
          | Some t, Some e ->
              (* Defined, so check arguments *)
              type_check_expression meta_ctx ctx type_ctx e t
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
          >>= fun _ -> Ok (Ast.Typ.TIdentifier constr_record.belongs_to_typ))
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
        type_check_expression meta_ctx ctx type_ctx expr typ >>= fun () ->
        Ok (Ast.Typ.TBox ([], typ))

let process_top_level meta_ctx ctx type_ctx = function
  | Ast.TopLevelDefn.Definition (iddef, e) ->
      let id, typ = iddef in
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx ctx type_ctx e typ >>= fun _ ->
      Ok
        ( Ast.TypedTopLevelDefn.Definition (typ, iddef, e),
          meta_ctx,
          new_ctx,
          type_ctx )
  | Ast.TopLevelDefn.RecursiveDefinition (iddef, e) ->
      let id, typ = iddef in
      let new_ctx = Typing_context.ObjTypingContext.add_mapping ctx id typ in
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx new_ctx type_ctx e typ >>= fun _ ->
      (*Note that here we type check in the new context (self reference)*)
      Ok
        ( Ast.TypedTopLevelDefn.RecursiveDefinition (typ, iddef, e),
          meta_ctx,
          new_ctx,
          type_ctx )
  | Ast.TopLevelDefn.Directive d ->
      let new_ctx =
        if Ast.Directive.equal d Ast.Directive.Reset then
          Typing_context.ObjTypingContext.create_empty_context ()
        else ctx
      in
      Ok (Ast.TypedTopLevelDefn.Directive d, meta_ctx, new_ctx, type_ctx)
  | Ast.TopLevelDefn.Expression e ->
      let open Or_error.Monad_infix in
      type_inference_expression meta_ctx ctx type_ctx e >>= fun typ ->
      Ok (Ast.TypedTopLevelDefn.Expression (typ, e), meta_ctx, ctx, type_ctx)
  | Ast.TopLevelDefn.DatatypeDecl (tid, constr_typ_list) ->
      let open Or_error.Monad_infix in
      Typing_context.TypeConstrTypingContext.add_typ_from_decl type_ctx
        (tid, constr_typ_list)
      >>= fun new_type_ctx ->
      Ok
        ( Ast.TypedTopLevelDefn.DatatypeDecl (tid, constr_typ_list),
          meta_ctx,
          ctx,
          new_type_ctx )

let rec type_check_program_aux meta_ctx ctx type_ctx program =
  match program with
  | [] -> Ok []
  | top :: tops ->
      let open Or_error.Monad_infix in
      process_top_level meta_ctx ctx type_ctx top
      >>= fun (typed_top, new_meta, new_ctx, new_type_ctx) ->
      type_check_program_aux new_meta new_ctx new_type_ctx tops
      >>= fun program_rest -> Ok (typed_top :: program_rest)

let type_check_program
    ?(meta_ctx = Typing_context.MetaTypingContext.create_empty_context ())
    ?(obj_ctx = Typing_context.ObjTypingContext.create_empty_context ())
    ?(type_ctx = Typing_context.TypeConstrTypingContext.empty) program =
  program |> type_check_program_aux meta_ctx obj_ctx type_ctx
