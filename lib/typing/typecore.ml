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
    ~(type_ctx : Typing_context.TypeConstrContext.t) ~(types_visited : TypSet.t)
    = function
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
          Typing_context.TypeConstrContext.get_constr_from_typ type_ctx tid
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

let includes_function_type ~(type_ctx : Typing_context.TypeConstrContext.t)
    ?(types_visited : TypSet.t = TypSet.empty) typ =
  let res, _ = includes_function_type_aux ~type_ctx ~types_visited typ in
  res

let rec is_valid_type type_ctx typevar_ctx typ ~current_type_depth =
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
      is_valid_type type_ctx typevar_ctx ~current_type_depth t1 >>= fun () ->
      is_valid_type type_ctx typevar_ctx ~current_type_depth t2
  | Ast.Typ.TBox (tvctx, ctx, t) ->
      let new_typevar_ctx =
        Typing_context.PolyTypeVarContext.add_all_mappings typevar_ctx
          (List.map ~f:(fun v -> (v, current_type_depth)) tvctx)
      in
      let new_current_type_depth =
        if Ast.TypeVarContext.is_empty tvctx then current_type_depth
        else current_type_depth + 1
      in
      List.map
        ~f:(fun (_, typ) ->
          is_valid_type type_ctx new_typevar_ctx typ
            ~current_type_depth:new_current_type_depth)
        ctx
      |> Or_error.combine_errors_unit
      >>= fun () ->
      is_valid_type type_ctx new_typevar_ctx
        ~current_type_depth:new_current_type_depth t
  | Ast.Typ.TProd tlist ->
      List.map tlist ~f:(fun t ->
          is_valid_type type_ctx typevar_ctx t ~current_type_depth)
      |> Or_error.combine_errors_unit
  | Ast.Typ.TRef t | Ast.Typ.TArray t ->
      is_valid_type type_ctx typevar_ctx ~current_type_depth t
  | Ast.Typ.TVar id -> (
      (* Get all mappings as list *)
      let id_str = id |> Ast.TypeVar.get_name in
      let debruijn_index =
        id |> Ast.TypeVar.get_debruijn_index
        |> Ast.DeBruijnIndex.value ~default:(-1)
        (* Default at -1 because this would NEVER match *)
      in
      let all_mappings =
        Typing_context.PolyTypeVarContext.get_all_mappings_as_list typevar_ctx
      in
      (* Check if defn level matches: if current level is L, and De Bruijn index of 0, then
         should be defined at level L-1 -- in general , at level L - D - 1 where D is the DB index.*)
      let matched =
        List.find all_mappings ~f:(fun (tvar, defn_level) ->
            String.equal (Ast.TypeVar.get_name tvar) id_str
            && defn_level = current_type_depth - debruijn_index - 1)
      in
      match matched with
      | None ->
          error
            "TypeValidityCheckError: The given type is invalid: unbound type \
             var ('[v], typevar_context, current_type_depth)"
            (id, typevar_ctx, current_type_depth)
            [%sexp_of:
              Ast.TypeVar.t * int Typing_context.PolyTypeVarContext.t * int]
      | Some (_, _) ->
          (* Check if defn level matches: if current level is L, and De Bruijn index of 0, then
             should be defined at level L-1 -- in general , at level L - D - 1 where D is the DB index.*)
          (* let debruijn_index =
               id |> Ast.TypeVar.get_debruijn_index
               |> Ast.DeBruijnIndex.value ~default:(-1)
               (* Default at -1 because this would NEVER match *)
             in *)
          Ok ())
  | Ast.Typ.TForall (id, typ) | Ast.Typ.TExists (id, typ) ->
      let new_typevar_context =
        Typing_context.PolyTypeVarContext.add_mapping typevar_ctx id
          current_type_depth
      in
      is_valid_type type_ctx new_typevar_context
        ~current_type_depth:(current_type_depth + 1) typ

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
    (typevar_ctx : int Typing_context.PolyTypeVarContext.t)
    ?(current_type_depth = 0) ?(allow_refs = false) expr typ =
  let open Or_error.Monad_infix in
  is_valid_type type_ctx typevar_ctx ~current_type_depth typ |> fun or_error ->
  Or_error.tag_arg or_error
    "TypeCheckError: Validity error for type given (typ, depth)"
    (typ, current_type_depth) [%sexp_of: Ast.Typ.t * int]
  >>= fun () ->
  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
    ~current_type_depth ~allow_refs expr
  >>= fun inferred_typ ->
  (*
     TODO: Dodgy equality check here: what if the DB indices aren't the same?
  *)
  if Ast.Typ.equal typ inferred_typ then Ok ()
  else
    error
      (Printf.sprintf
         "TypeCheckError: Inferred type\n\
         \          %s\n\
         \ Not Equal to checked type\n\
         \         %s. \n\
         \         (expr, inferred_typ, typ, ctx, meta_ctx)"
         (Ast.Typ.pretty_print inferred_typ)
         (Ast.Typ.pretty_print typ))
      (expr, inferred_typ, typ, ctx, meta_ctx)
      [%sexp_of:
        Ast.Expr.t
        * Ast.Typ.t
        * Ast.Typ.t
        * (Ast.Typ.t * int) Typing_context.ObjTypingContext.t
        * (Ast.TypeVarContext.t * Ast.Context.t * Ast.Typ.t * int)
          Typing_context.MetaTypingContext.t]

and type_inference_expression meta_ctx ctx type_ctx typevar_ctx
    ?(current_type_depth = 0) ?(allow_refs = false) e =
  (* Invariant: type_inference at type_depth d should yield DB indices which correspond to this exact type depth *)
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
              (Ast.Typ.t * int) Typing_context.ObjTypingContext.t
              * Ast.ObjIdentifier.t]
      | Some (typ, type_depth) ->
          (* Shift to match the current depth *)
          Ast.Typ.shift_indices typ ~type_depth:0
            ~type_offset:(current_type_depth - type_depth)
          |> fun or_error ->
          Or_error.tag_arg or_error
            "TypeInferenceError: Error at index correction for object variable."
            (Ast.Expr.Identifier id) [%sexp_of: Ast.Expr.t]
          >>= fun typ -> Ok typ)
  | Ast.Expr.Constant c -> (
      match c with
      | Ast.Constant.Boolean _ -> Ok Ast.Typ.TBool
      | Ast.Constant.Integer _ -> Ok Ast.Typ.TInt
      | Ast.Constant.Unit -> Ok Ast.Typ.TUnit
      | Ast.Constant.Character _ -> Ok Ast.Typ.TChar
      | Ast.Constant.String _ -> Ok Ast.Typ.TString
      | Ast.Constant.Array arr_cell ->
          if not allow_refs then
            error
              "TypeInferenceError: Can't type check a reference or an array \
               constant"
              c [%sexp_of: Ast.Constant.t]
          else
            let open Ast.ArrayCell in
            (* Safe because we disallow empty arrays *)
            type_inference_expression meta_ctx ctx type_ctx typevar_ctx
              ~current_type_depth ~allow_refs
              (Ast.Value.to_expr (get arr_cell 0))
            >>= fun typ -> Ok (Ast.Typ.TArray typ)
      | Ast.Constant.Reference ref_cell ->
          if not allow_refs then
            error
              "TypeInferenceError: Can't type check a reference or an array \
               constant"
              c [%sexp_of: Ast.Constant.t]
          else
            let open Ast.RefCell in
            (* Safe because we disallow empty arrays *)
            type_inference_expression meta_ctx ctx type_ctx typevar_ctx
              ~current_type_depth ~allow_refs
              (Ast.Value.to_expr !ref_cell)
            >>= fun typ -> Ok (Ast.Typ.TRef typ))
  | Ast.Expr.UnaryOp (op, expr) -> (
      match op with
      | Ast.UnaryOperator.NEG ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx ~allow_refs
            ~current_type_depth expr Ast.Typ.TInt
          >>= fun () -> Ok Ast.Typ.TInt
      | Ast.UnaryOperator.NOT ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr Ast.Typ.TBool
          >>= fun () -> Ok Ast.Typ.TBool
      | Ast.UnaryOperator.DEREF -> (
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr
          >>= fun typ ->
          match typ with
          | Ast.Typ.TRef typ -> Ok typ
          | _ ->
              Or_error.error
                "TypeInferenceError: Dereferencing a non-reference value \
                 (expr, typ)"
                (expr, typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t])
      | Ast.UnaryOperator.ARRAY_LEN -> (
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr
          >>= function
          | Ast.Typ.TArray _ -> Ok Ast.Typ.TInt
          | typ ->
              Or_error.error
                "TypeInferenceError: Getting length of a non-array value \
                 (expr, typ)"
                (expr, typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t]))
  | Ast.Expr.BinaryOp (op, expr, expr2) -> (
      let check_both typ_to_check ok_typ =
        type_check_expression meta_ctx ctx type_ctx typevar_ctx
          ~current_type_depth ~allow_refs expr typ_to_check
        >>= fun () ->
        type_check_expression meta_ctx ctx type_ctx typevar_ctx
          ~current_type_depth ~allow_refs expr2 typ_to_check
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
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr
          >>= fun typ ->
          if includes_function_type ~type_ctx typ then
            Or_error.error
              "TypeInferenceError: Can't decide equality between types \
               containing functional types."
              typ [%sexp_of: Ast.Typ.t]
          else
            let or_error =
              type_check_expression meta_ctx ctx type_ctx typevar_ctx
                ~current_type_depth ~allow_refs expr2 typ
            in
            Or_error.tag or_error
              ~tag:
                ("TypeInferenceError: Type mismatch: both sides of equality \
                  must have same type: " ^ Ast.Typ.show typ)
            >>= fun _ -> Ok Ast.Typ.TBool
      | Ast.BinaryOperator.NEQ ->
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr
          >>= fun typ ->
          let or_error =
            type_check_expression meta_ctx ctx type_ctx typevar_ctx
              ~current_type_depth ~allow_refs expr2 typ
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
          type_check_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr Ast.Typ.TChar
          >>= fun () ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr2 Ast.Typ.TString
          >>= fun () -> Ok Ast.Typ.TString
      | Ast.BinaryOperator.STRINGCONCAT ->
          check_both Ast.Typ.TString Ast.Typ.TString
      | Ast.BinaryOperator.SEQ ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr Ast.Typ.TUnit
          >>= fun () ->
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr2
      | Ast.BinaryOperator.ASSIGN -> (
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr
          >>= fun t1 ->
          match t1 with
          | Ast.Typ.TRef typ ->
              (* Now check typ of second value *)
              ( type_check_expression meta_ctx ctx type_ctx typevar_ctx
                  ~current_type_depth ~allow_refs expr2 typ
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
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs expr
          >>= function
          | Ast.Typ.TArray val_typ ->
              type_check_expression meta_ctx ctx type_ctx typevar_ctx
                ~current_type_depth ~allow_refs expr2 Ast.Typ.TInt
              |> Or_error.tag
                   ~tag:"TypeInferenceError: array index not of int type"
              >>= fun () -> Ok val_typ
          | _ ->
              Or_error.error
                "TypeInferenceError: Can't index on non-array values [arr]" expr
                [%sexp_of: Ast.Expr.t]))
  | Ast.Expr.Prod exprs ->
      List.map exprs
        ~f:
          (type_inference_expression meta_ctx ctx type_ctx typevar_ctx
             ~current_type_depth ~allow_refs)
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
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs expr
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
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx
           ~current_type_depth ~allow_refs expr t1)
        ~tag:
          ("TypeInferenceError: the given expression must be of the left type \
            of the given sum type: " ^ Ast.Typ.show t1)
      >>= fun () -> Ok (Ast.Typ.TSum (t1, t2))
  | Ast.Expr.Right (t1, t2, expr) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx
           ~current_type_depth ~allow_refs expr t2)
        ~tag:
          ("TypeInferenceError: the given expression must be of of the right \
            type of the given sum type: " ^ Ast.Typ.show t2)
      >>= fun () -> Ok (Ast.Typ.TSum (t1, t2))
  | Ast.Expr.Case (e, iddef1, e1, iddef2, e2) ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e
      >>= fun e_typ ->
      (match e_typ with
      | Ast.Typ.TSum (_, _) -> Ok ()
      | typ ->
          error "TypeInferenceError: argument of `match` must be of a sum type."
            typ [%sexp_of: Ast.Typ.t])
      >>= fun () ->
      let id1, typ1 = iddef1 in
      let new_ctx1 =
        Typing_context.ObjTypingContext.add_mapping ctx id1
          (typ1, current_type_depth)
      in
      type_inference_expression meta_ctx new_ctx1 type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e1
      >>= fun e1_type ->
      let id2, typ2 = iddef2 in
      let new_ctx2 =
        Typing_context.ObjTypingContext.add_mapping ctx id2
          (typ2, current_type_depth)
      in
      (*Types should match, or there is a Type mismatch error*)
      let or_error =
        type_check_expression meta_ctx new_ctx2 type_ctx typevar_ctx
          ~current_type_depth ~allow_refs e2 e1_type
      in
      Or_error.tag or_error
        ~tag:
          "TypeInferenceError: Type mismatch: Each outcome of a match clause \
           must give the same type"
      >>= fun () -> Ok e1_type
  | Ast.Expr.Lambda ((id, typ), e) ->
      let new_ctx =
        Typing_context.ObjTypingContext.add_mapping ctx id
          (typ, current_type_depth)
      in
      type_inference_expression meta_ctx new_ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e
      >>= fun e_typ -> Ok (Ast.Typ.TFun (typ, e_typ))
  | Ast.Expr.Application (e1, e2) ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e1
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
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx
           ~current_type_depth ~allow_refs e2 arg_typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: Type mismatch: expected argument of type \n\
             \             %s."
             (Ast.Typ.pretty_print arg_typ))
      >>= fun () -> Ok res_typ
  | Ast.Expr.IfThenElse (b, e1, e2) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx
           ~current_type_depth ~allow_refs b Ast.Typ.TBool)
        ~tag:
          "TypeInferenceError: the predicate of an if-then-else expression \
           must be of boolean type"
      >>= fun () ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e1
      >>= fun e1_typ ->
      Or_error.tag_arg
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx
           ~current_type_depth ~allow_refs e2 e1_typ)
        "TypeInferenceError: All outcomes of an if-then-else expression must \
         be of the same type."
        (e2, e1_typ) [%sexp_of: Ast.Expr.t * Ast.Typ.t]
      >>= fun () -> Ok e1_typ
  | Ast.Expr.LetBinding ((id, typ), e, e2) ->
      Or_error.tag
        (type_check_expression meta_ctx ctx type_ctx typevar_ctx
           ~current_type_depth ~allow_refs e typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: variable %s is declared of type \n\
             \                %s \n\
             \              but bound to an expression of a different type"
             (Ast.ObjIdentifier.show id)
             (Ast.Typ.pretty_print typ))
      >>= fun () ->
      let new_ctx =
        Typing_context.ObjTypingContext.add_mapping ctx id
          (typ, current_type_depth)
      in
      type_inference_expression meta_ctx new_ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e2
  | Ast.Expr.LetRec ((id, typ), e, e2) ->
      (*We only allow types which are of the form A->B*)
      is_valid_type_for_recursion typ >>= fun () ->
      let new_ctx =
        Typing_context.ObjTypingContext.add_mapping ctx id
          (typ, current_type_depth)
      in
      Or_error.tag
        (type_check_expression meta_ctx new_ctx type_ctx typevar_ctx
           ~current_type_depth ~allow_refs e typ)
        ~tag:
          (Printf.sprintf
             "TypeInferenceError: recursive variable %s is declared of type \n\
             \              %s \n\
             \              but bound to an expression of a different type"
             (Ast.ObjIdentifier.show id)
             (Ast.Typ.pretty_print typ))
      >>= fun () ->
      type_inference_expression meta_ctx new_ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e2
  | Ast.Expr.LetRecMutual (idddef_e_list, e2) ->
      (* First get the types *)
      let iddefs, _ = List.unzip idddef_e_list in
      (* Then check the types are functional *)
      List.map iddefs ~f:(fun (_, typ) -> is_valid_type_for_recursion typ)
      |> Or_error.combine_errors_unit
      >>= fun () ->
      let new_ctx =
        Typing_context.ObjTypingContext.add_all_mappings ctx
          (List.map iddefs ~f:(fun (id, typ) -> (id, (typ, current_type_depth))))
      in
      List.map idddef_e_list ~f:(fun ((id, typ), e) ->
          type_check_expression meta_ctx new_ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs e typ
          |> Or_error.tag
               ~tag:
                 (Printf.sprintf
                    "TypeInferenceError: recursive variable %s is declared of \
                     type \n\
                    \                     %s \n\
                    \                     but bound to an expression of a \
                     different type"
                    (Ast.ObjIdentifier.show id)
                    (Ast.Typ.pretty_print typ)))
      |> Or_error.combine_errors_unit
      |> Or_error.tag
           ~tag:
             "TypeInferenceError: Type error at mutually recursive declaration"
      >>= fun () ->
      type_inference_expression meta_ctx new_ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e2
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
          (List.map box_tvctx ~f:(fun v -> (v, current_type_depth)))
      in
      (* Compute the new depth for types in the context *)
      let new_type_depth =
        if Ast.TypeVarContext.is_empty box_tvctx then current_type_depth
        else current_type_depth + 1
      in
      let new_ctx =
        Typing_context.ObjTypingContext.add_all_mappings
          (Typing_context.ObjTypingContext.create_empty_context ())
          (List.map box_context ~f:(fun (id, typ) ->
               (id, (typ, new_type_depth))))
      in
      type_inference_expression meta_ctx new_ctx type_ctx new_typevar_ctx
        ~current_type_depth:new_type_depth ~allow_refs e
      >>= fun e_typ -> Ok (Ast.Typ.TBox (box_tvctx, box_context, e_typ))
  | Ast.Expr.LetBox (metaid, e, e2) ->
      ( (*First check that e gives a box*)
        type_inference_expression meta_ctx ctx type_ctx typevar_ctx
          ~current_type_depth ~allow_refs e
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
          (box_tvctx, box_context, box_typ, current_type_depth)
      in
      type_inference_expression new_meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e2
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
              (Ast.TypeVarContext.t * Ast.Context.t * Ast.Typ.t * int)
              Typing_context.MetaTypingContext.t
              * Ast.MetaIdentifier.t]
      | Some (box_tvctx, box_context, box_typ, box_type_depth) ->
          (* Do the shifting: if the box_tvctx is not empty then
             the type_depth to work at is 1, otherwise it's 0.
          *)
          let type_depth_to_shift =
            if Ast.TypeVarContext.is_empty box_tvctx then 0 else 1
          in
          ( Ast.Context.shift_indices ~type_depth:type_depth_to_shift
              ~type_offset:(current_type_depth - box_type_depth)
              box_context
          >>= fun box_context ->
            Ast.Typ.shift_indices ~type_depth:type_depth_to_shift
              ~type_offset:(current_type_depth - box_type_depth)
              box_typ
            >>= fun box_typ -> Ok (box_tvctx, box_context, box_typ) )
          |> fun or_error ->
          Or_error.tag_arg or_error
            "TypeInferenceError: Error at index correction for closure expr"
            (Ast.Expr.Closure (meta_id, typs, exprs))
            [%sexp_of: Ast.Expr.t])
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
        if not (List.is_empty typs) then
          (* 2- check types *)
          Or_error.tag
            (Or_error.combine_errors_unit
               (List.map zipped_list ~f:(fun ((_, typ), e) ->
                    (* Subsitute type *)
                    Substitutions.sim_type_type_substitute typs box_tvctx typ
                    >>= fun typ ->
                    type_check_expression meta_ctx ctx type_ctx typevar_ctx
                      ~current_type_depth ~allow_refs e typ)))
            ~tag:
              "TypeInferenceError: Type mismatch between context and \
               expressions provided to substitute in."
          >>= fun () ->
          (* Now substitute the typ *)
          Substitutions.sim_type_type_substitute typs box_tvctx box_typ
        else
          Or_error.tag
            (Or_error.combine_errors_unit
               (List.map zipped_list ~f:(fun ((_, typ), e) ->
                    (* Subsitute type *)
                    type_check_expression meta_ctx ctx type_ctx typevar_ctx
                      ~current_type_depth ~allow_refs e typ)))
            ~tag:
              "TypeInferenceError: Type mismatch between context and \
               expressions provided to substitute in."
          >>= fun () -> Ok box_typ
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
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e
      >>= fun inferred_typ ->
      (* print_endline (Ast.Typ.pretty_print inferred_typ); *)
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
                      (List.map zipped_list ~f:(fun (id, typ) ->
                           (id, (typ, current_type_depth))))
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx ~current_type_depth ~allow_refs expr
              | Ast.Pattern.Id id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      (inferred_typ, current_type_depth)
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx ~current_type_depth ~allow_refs expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    ~current_type_depth ~allow_refs expr
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
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      (t1, current_type_depth)
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx ~current_type_depth ~allow_refs expr
              | Ast.Pattern.Inr id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      (t2, current_type_depth)
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx ~current_type_depth ~allow_refs expr
              | Ast.Pattern.Id id ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_mapping ctx id
                      (inferred_typ, current_type_depth)
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx ~current_type_depth ~allow_refs expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    ~current_type_depth ~allow_refs expr
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
                      (inferred_typ, current_type_depth)
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx ~current_type_depth ~allow_refs expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    ~current_type_depth ~allow_refs expr
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
                  (* print_endline ([%sexp_of: Ast.Typ.t list] typs |> Sexp.to_string_hum); *)
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
                  (* print_endline ([%sexp_of: Ast.Typ.t list] typs |> Sexp.to_string_hum); *)
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
                      (List.map zipped_list ~f:(fun (id, typ) ->
                           (id, (typ, current_type_depth))))
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx ~current_type_depth ~allow_refs expr
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
                      (inferred_typ, current_type_depth)
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    typevar_ctx ~current_type_depth ~allow_refs expr
              | Ast.Pattern.Wildcard ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    ~current_type_depth ~allow_refs expr
              | Ast.Pattern.String _ ->
                  type_inference_expression meta_ctx ctx type_ctx typevar_ctx
                    ~current_type_depth ~allow_refs expr
              | Ast.Pattern.ConcatCharString (cid, sid) ->
                  let new_ctx =
                    Typing_context.ObjTypingContext.add_all_mappings ctx
                      [
                        (cid, (Ast.Typ.TChar, current_type_depth));
                        (sid, (Ast.Typ.TString, current_type_depth));
                      ]
                  in
                  type_inference_expression meta_ctx new_ctx type_ctx
                    ~current_type_depth ~allow_refs typevar_ctx expr
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
        Typing_context.TypeConstrContext.get_typ_from_constr type_ctx constr
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
              |> Or_error.tag
                   ~tag:
                     (Printf.sprintf
                        "TypeInferenceError: Simultaneous type substitution \
                         error on Constructor %s."
                        (Ast.Constructor.get_name constr))
              >>= fun t ->
              (* Defined, so check arguments *)
              type_check_expression meta_ctx ctx type_ctx typevar_ctx
                ~current_type_depth ~allow_refs e t
              |> Or_error.tag
                   ~tag:
                     (Printf.sprintf
                        "TypeInferenceError: On Constructor %s: argument type \
                         mismatch. Expected type %s"
                        (Ast.Constructor.get_name constr)
                        (Ast.Typ.pretty_print t))
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
        type_check_expression meta_ctx ctx type_ctx typevar_ctx
          ~current_type_depth ~allow_refs expr typ
        >>= fun () -> Ok (Ast.Typ.TBox ([], [], typ))
  | Ast.Expr.EValue v ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs (Ast.Value.to_expr v)
  | Ast.Expr.Ref expr ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs expr
      >>= fun typ -> Ok (Ast.Typ.TRef typ)
  | Ast.Expr.While (p, e) ->
      type_check_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs p Ast.Typ.TBool
      |> Or_error.tag
           ~tag:
             "TypeInferenceError: Expected bool type for while loop predicate."
      >>= fun () ->
      type_check_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e Ast.Typ.TUnit
      |> Or_error.tag
           ~tag:"TypeInferenceError: Expected unit type body for while loops"
      >>= fun () -> Ok Ast.Typ.TUnit
  | Ast.Expr.Array es -> (
      match es with
      | [] ->
          Or_error.error "TypeInferenceError: Array is empty"
            (Ast.Expr.Array es) [%sexp_of: Ast.Expr.t]
      | x :: xs ->
          type_inference_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs x
          >>= fun x_typ ->
          (* Check that all other ones are of the same type *)
          List.map xs ~f:(fun x ->
              type_check_expression meta_ctx ctx type_ctx typevar_ctx
                ~allow_refs ~current_type_depth x x_typ)
          |> Or_error.combine_errors_unit
          >>= fun () -> Ok (Ast.Typ.TArray x_typ))
  | Ast.Expr.ArrayAssign (arr, index, e) -> (
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs arr
      >>= function
      | Ast.Typ.TArray val_typ ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth ~allow_refs index Ast.Typ.TInt
          |> fun or_error ->
          Or_error.tag_arg or_error
            "TypeInferenceError: At Array Assignment: index not of type int \
             [index]"
            index [%sexp_of: Ast.Expr.t]
          >>= fun () ->
          type_check_expression meta_ctx ctx type_ctx typevar_ctx
            ~current_type_depth e val_typ ~allow_refs
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
        Typing_context.PolyTypeVarContext.add_mapping typevar_ctx v
          current_type_depth
      in
      type_inference_expression meta_ctx ctx type_ctx new_typevar_ctx
        ~current_type_depth:(current_type_depth + 1) ~allow_refs e
      >>= fun typ -> Ok (Ast.Typ.TForall (v, typ))
  | Ast.Expr.TypeApply (e, t) -> (
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e
      >>= fun e_typ ->
      (* Check is right type (big lambda) *)
      match e_typ with
      | Ast.Typ.TForall (v, typ) ->
          (* Check type validity *)
          is_valid_type type_ctx typevar_ctx ~current_type_depth t >>= fun () ->
          Substitutions.type_type_substitute t v typ
      | _ ->
          error
            "TypeInferenceError: At type apply, the left operand must be a big \
             lambda term. (left, right)"
            (e, t) [%sexp_of: Ast.Expr.t * Ast.Typ.t])
  | Ast.Expr.Pack ((i_tv, i_typ), typ, e) ->
      is_valid_type type_ctx
        (Typing_context.PolyTypeVarContext.add_mapping typevar_ctx i_tv
           current_type_depth)
        ~current_type_depth:(current_type_depth + 1) i_typ
      >>= fun () ->
      is_valid_type type_ctx typevar_ctx ~current_type_depth typ >>= fun () ->
      Substitutions.type_type_substitute typ i_tv i_typ
      |> Or_error.tag
           ~tag:
             "TypeInferenceError: At Pack, error when substituting the hidden \
              type in the interface"
      >>= fun e_typ ->
      type_check_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth ~allow_refs e e_typ
      |> Or_error.tag
           ~tag:
             "TypeInferenceError: at Pack, mismatch between interface type and \
              implementation type."
      >>= fun () -> Ok (Ast.Typ.TExists (i_tv, i_typ))
  | Ast.Expr.LetPack (tv, oid, e1, e2) ->
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx ~allow_refs
        ~current_type_depth e1
      >>= fun e1_typ ->
      (* print_endline (Ast.Typ.pretty_print e1_typ); *)
      (match e1_typ with
      | Ast.Typ.TExists (exists_tv, exists_typ) -> Ok (exists_tv, exists_typ)
      | _ ->
          Or_error.error
            "TypeInferenceError: Can only unpack packed values. (expr)"
            (Ast.Expr.LetPack (tv, oid, e1, e2))
            [%sexp_of: Ast.Expr.t])
      >>= fun (exists_tv, exists_typ) ->
      (* Substitute tv for exists_tv in exists_typ so we have an existential
         typ that depends on tv. *)
      Ast.DeBruijnIndex.create 0 >>= fun db_index ->
      (* print_endline (Ast.Typ.pretty_print exists_typ); *)
      (* Here all we want is alpha renaming, but substitution assumes
         type-application-like semantics, so the solution is
         the shift everything apart from depth 0 up, and then
          do the substitution. Shift with type depth 1 so that we don't touch
         anything with index 0. *)
      Ast.Typ.shift_indices exists_typ ~type_depth:1 ~type_offset:1
      >>= fun exists_typ ->
      Substitutions.type_type_substitute
        (Ast.Typ.TVar
           (Ast.TypeVar.of_string_and_index (Ast.TypeVar.get_name tv) db_index))
        exists_tv exists_typ
      >>= fun new_exists_typ ->
      (* print_endline (Ast.Typ.pretty_print new_exists_typ); *)
      let new_typevar_ctx =
        Typing_context.PolyTypeVarContext.add_mapping typevar_ctx tv
          current_type_depth
      in
      let new_type_depth = current_type_depth + 1 in
      let new_obj_ctx =
        Typing_context.ObjTypingContext.add_mapping ctx oid
          (new_exists_typ, new_type_depth)
        (* This new type depends on the typevar declaration for `tv` *)
      in
      type_inference_expression meta_ctx new_obj_ctx type_ctx new_typevar_ctx
        ~current_type_depth:new_type_depth ~allow_refs e2
      |> fun or_error ->
      Or_error.tag_arg or_error
        "TypeInferenceError: at let pack, error at infering the body \
         expression. (new_obj_ctx, new_typevar_ctx, new_type_depth, e2)"
        (new_obj_ctx, new_typevar_ctx, new_type_depth, e2)
        [%sexp_of:
          (Ast.Typ.t * int) Typing_context.ObjTypingContext.t
          * int Typing_context.PolyTypeVarContext.t
          * int
          * Ast.Expr.t]
      >>= fun e2_typ ->
      (* the hidden type mustn't go out -- could loosen *)
      is_valid_type type_ctx typevar_ctx
        ~current_type_depth:(current_type_depth + 1) e2_typ
      |> fun or_error ->
      Or_error.tag_arg or_error
        "TypeInferenceError: let pack in result type must NOT depend on the \
         packed type."
        (Ast.Expr.LetPack (tv, oid, e1, e2))
        [%sexp_of: Ast.Expr.t]
      >>= fun () ->
      (* Now e2_typ is valid at level current_type_depth + 1, but
         we know it doesn't depend on types at that depth so we shift the indices
         >= 1 (so above the index added because of the pack) back
         by 1 to get the type at current_type_depth *)
      Ast.Typ.shift_indices e2_typ ~type_depth:1 ~type_offset:(-1)
      >>= fun e2_typ -> Ok e2_typ

let process_top_level meta_ctx ctx type_ctx typevar_ctx ?(allow_refs = false) =
  function
  | Ast.TopLevelDefn.Definition (iddef, e) ->
      let id, typ = iddef in
      let new_ctx =
        Typing_context.ObjTypingContext.add_mapping ctx id (typ, 0)
      in
      (* 0 not because it is right, but because the type at the top level must be closed, so it doesn't matter which level we choose. *)
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx ctx type_ctx typevar_ctx ~allow_refs
        ~current_type_depth:0 e typ
      >>= fun _ ->
      Ok
        ( Ast.TypedTopLevelDefn.Definition (typ, iddef, e),
          meta_ctx,
          new_ctx,
          type_ctx,
          typevar_ctx )
  | Ast.TopLevelDefn.RecursiveDefinition (iddef, e) ->
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx ctx type_ctx typevar_ctx ~allow_refs
        ~current_type_depth:0
        (Ast.Expr.LetRec (iddef, e, Ast.Expr.Constant Ast.Constant.Unit))
        Ast.Typ.TUnit
      >>= fun _ ->
      (*Note that here we type check in the new context (self reference)*)
      let id, typ = iddef in
      let new_ctx =
        Typing_context.ObjTypingContext.add_mapping ctx id (typ, 0)
      in
      (* Like the above *)
      Ok
        ( Ast.TypedTopLevelDefn.RecursiveDefinition (typ, iddef, e),
          meta_ctx,
          new_ctx,
          type_ctx,
          typevar_ctx )
  | Ast.TopLevelDefn.MutualRecursiveDefinition iddef_e_list ->
      let open Or_error.Monad_infix in
      type_check_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth:0 ~allow_refs
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
          (List.map iddef_e_list ~f:(fun ((id, typ), _) -> (id, (typ, 0))))
        (* Like the above *)
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
      type_inference_expression meta_ctx ctx type_ctx typevar_ctx
        ~current_type_depth:0 ~allow_refs e
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
          Typing_context.TypeConstrContext.add_typ_from_decl running_type_ctx
            (tvctx, tid, constr_typ_list))
      >>= fun new_type_ctx ->
      Ok
        ( Ast.TypedTopLevelDefn.DatatypeDecl id_constr_typ_list_list,
          meta_ctx,
          ctx,
          new_type_ctx,
          typevar_ctx )

let rec type_check_program_aux meta_ctx ctx type_ctx typevar_ctx ~allow_refs
    program =
  match program with
  | [] -> Ok []
  | top :: tops ->
      let open Or_error.Monad_infix in
      process_top_level meta_ctx ctx type_ctx typevar_ctx ~allow_refs top
      >>= fun (typed_top, new_meta, new_ctx, new_type_ctx, new_typevar_ctx) ->
      type_check_program_aux new_meta new_ctx new_type_ctx new_typevar_ctx
        ~allow_refs tops
      >>= fun program_rest -> Ok (typed_top :: program_rest)

let type_check_program
    ?(meta_ctx = Typing_context.MetaTypingContext.create_empty_context ())
    ?(obj_ctx = Typing_context.ObjTypingContext.create_empty_context ())
    ?(type_ctx = Typing_context.TypeConstrContext.empty)
    ?(typevar_ctx = Typing_context.PolyTypeVarContext.create_empty_context ())
    ?(allow_refs = false) program =
  program
  |> type_check_program_aux meta_ctx obj_ctx type_ctx typevar_ctx ~allow_refs
