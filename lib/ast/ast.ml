open Core

(*Map from Past.Identifier to anything*)

module DeBruijnIndex : sig
  (*Implementation of De Bruijn Indices -- encapsulated*)
  type t [@@deriving sexp, show, compare, equal]

  val none : t
  val top_level : t
  val create : int -> t Or_error.t
  val shift : t -> int -> int -> t Or_error.t
  val value : t -> default:int -> int
end = struct
  type t = NotSet | TopLevel | InExpr of int
  [@@deriving sexp, show, compare, equal]

  let none = NotSet
  let top_level = TopLevel

  let create v =
    if v >= 0 then Ok (InExpr v)
    else
      error "DeBruijnIndexError: De Bruijn Index must be >= 0." v
        [%sexp_of: int]

  let shift i depth k =
    (* Can't shift down *)
    (* if k < 0 then
         error "DeBruijnIndexError: Shifting failed: the offset must be positive."
           k [%sexp_of: int]
       else *)
    (* Only shift IF the index is >= the depth *)
    match i with
    | InExpr v ->
        if v < depth then Ok (InExpr v)
        else if v + k >= 0 then Ok (InExpr (v + k))
        else
          error
            "DeBruijnIndexError: Shifting failed: the value of the index can't \
             be negative."
            (v, k) [%sexp_of: int * int]
    | _ -> Ok i

  let value x ~default = match x with InExpr v -> v | _ -> default
end

module type Identifier_type = sig
  module PastModule : sig
    type t [@@deriving sexp, show, compare, equal]
  end

  type t [@@deriving sexp, show, compare, equal]

  val of_string : string -> t
  val of_past : PastModule.t -> t
  val of_string_and_index : string -> DeBruijnIndex.t -> t
  val get_name : t -> string
  val get_debruijn_index : t -> DeBruijnIndex.t

  val populate_index :
    t ->
    current_level:int ->
    current_identifiers:int String.Map.t ->
    t Or_error.t

  val shift : t -> depth:int -> offset:int -> t Or_error.t
end

module Identifier_impl =
functor
  (PastModule : sig
     type t [@@deriving sexp, show, compare, equal]
   end)
  ->
  struct
    module PastModule = PastModule

    type t = string * DeBruijnIndex.t [@@deriving sexp, show, compare, equal]

    let of_string x = (x, DeBruijnIndex.none)
    (* Dummy init at -1. TODO: Change design *)

    let of_past (past_identifier : PastModule.t) =
      (past_identifier, DeBruijnIndex.none)

    let of_string_and_index str index = (str, index)
    let get_name (id, _) = id
    let get_debruijn_index (_, index) = index

    let populate_index (id, _) ~current_level ~current_identifiers =
      (* The concept of the De Bruijn index thing is to remember the current level. *)
      (* If ~current_ast_level or ~current_identifiers not given, assume that we're talking about an identifier definition, so the index returned should be Index.none *)
      let open Or_error.Monad_infix in
      let level_opt = String.Map.find current_identifiers id in
      (*Get the level at which id is defined.*)
      match level_opt with
      | None ->
          Ok (id, DeBruijnIndex.top_level)
          (*Assume top level if not in context.*)
      | Some lvl ->
          DeBruijnIndex.create (current_level - lvl - 1)
          |> Or_error.tag
               ~tag:
                 (Printf.sprintf
                    "DeBruijnPopulationError: failed to create De Bruijn index \
                     for %s"
                    id)
          >>= fun i -> Ok (id, i)

    (*-1 because we have levels as the number of binders the current construct is under e.g. fun x (level 0) -> fun y (level 1) -> x (level 2) + y (level 2)*)
    let shift (id, index) ~depth ~offset =
      let open Or_error.Monad_infix in
      DeBruijnIndex.shift index depth offset >>= fun new_index ->
      Ok (id, new_index)
  end

module type ObjIdentifier_type =
  Identifier_type with module PastModule = Past.Identifier

module type Constructor_type = sig
  type t [@@deriving sexp, show, compare, equal]

  val of_string : string -> t
  val of_past : Past.Constructor.t -> t
  val get_name : t -> string
end

module type TypeIdentifier_type = sig
  (*Unused for now*)
  type t [@@deriving sexp, show, compare, equal]

  val get_name : t -> string
  val of_string : string -> t
  val of_past : Past.Identifier.t -> t
end

module type MetaIdentifier_type =
  Identifier_type with module PastModule = Past.Identifier

module type TypeVar_type = Identifier_type with module PastModule = Past.TypeVar

module rec ObjIdentifier : ObjIdentifier_type = Identifier_impl (Past.Identifier)
and MetaIdentifier : MetaIdentifier_type = Identifier_impl (Past.Identifier)

and TypeIdentifier : TypeIdentifier_type = struct
  type t = string [@@deriving sexp, show, compare, equal]

  let get_name v = v
  let of_string x = x
  let of_past (past_identifier : Past.Identifier.t) = past_identifier
end

and Constructor : Constructor_type = struct
  type t = string [@@deriving sexp, show, compare, equal]

  let get_name v = v
  let of_string v = v
  let of_past (past_constructor : Past.Constructor.t) = past_constructor
end

and TypeVar : TypeVar_type = Identifier_impl (Past.TypeVar)

and Typ : sig
  type t =
    | TUnit
    | TBool
    | TInt
    | TChar
    | TString
    | TIdentifier of t list * TypeIdentifier.t
    | TFun of t * t
    | TBox of TypeVarContext.t * Context.t * t
    | TProd of t list
    | TSum of t * t
    | TRef of t
    | TArray of t
    | TVar of TypeVar.t
    | TForall of TypeVar.t * t
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.Typ.t -> t

  val populate_index :
    t ->
    current_type_ast_level:int ->
    current_typevars:int String.Map.t ->
    t Or_error.t

  val shift_indices : t -> type_depth:int -> type_offset:int -> t Or_error.t
end = struct
  type t =
    | TUnit
    | TBool
    | TInt
    | TChar
    | TString
    | TIdentifier of t list * TypeIdentifier.t
    | TFun of t * t
    | TBox of TypeVarContext.t * Context.t * t
    | TProd of t list
    | TSum of t * t
    | TRef of t
    | TArray of t
    | TVar of TypeVar.t
    | TForall of TypeVar.t * t
  [@@deriving sexp, show, compare, equal]

  let rec of_past = function
    | Past.Typ.TUnit -> TUnit
    | Past.Typ.TBool -> TBool
    | Past.Typ.TInt -> TInt
    | Past.Typ.TChar -> TChar
    | Past.Typ.TString -> TString
    | Past.Typ.TIdentifier (tlist, id) ->
        TIdentifier (List.map ~f:of_past tlist, TypeIdentifier.of_past id)
    | Past.Typ.TFun (t1, t2) -> TFun (of_past t1, of_past t2)
    | Past.Typ.TBox (typevarctx, ctx, t1) ->
        TBox (TypeVarContext.of_past typevarctx, Context.of_past ctx, of_past t1)
    | Past.Typ.TProd ts -> TProd (List.map ts ~f:of_past)
    | Past.Typ.TSum (t1, t2) -> TSum (of_past t1, of_past t2)
    | Past.Typ.TRef t -> TRef (of_past t)
    | Past.Typ.TArray t -> TArray (of_past t)
    | Past.Typ.TForall (v, t) -> TForall (TypeVar.of_past v, of_past t)
    | Past.Typ.TVar v -> TVar (TypeVar.of_past v)

  let rec populate_index typ ~current_type_ast_level ~current_typevars =
    let open Or_error.Monad_infix in
    match typ with
    | TUnit -> Ok TUnit
    | TBool -> Ok TBool
    | TInt -> Ok TInt
    | TChar -> Ok TChar
    | TString -> Ok TString
    | TIdentifier (tlist, id) ->
        List.map
          ~f:(populate_index ~current_type_ast_level ~current_typevars)
          tlist
        |> Or_error.combine_errors
        >>= fun tlist -> Ok (TIdentifier (tlist, id))
    | TFun (t1, t2) ->
        populate_index ~current_type_ast_level ~current_typevars t1
        >>= fun t1 ->
        populate_index ~current_type_ast_level ~current_typevars t2
        >>= fun t2 -> Ok (TFun (t1, t2))
    | TBox (tvctx, ctx, t) ->
        let new_typ_level, new_typvars =
          if TypeVarContext.is_empty tvctx then
            (* If empty then don't add a level. *)
            (current_type_ast_level, current_typevars)
          else
            (* Insert context and add level *)
            ( current_type_ast_level + 1,
              List.fold tvctx ~init:current_typevars ~f:(fun acc tv ->
                  String.Map.set acc ~key:(TypeVar.get_name tv)
                    ~data:current_type_ast_level) )
        in
        List.map ctx ~f:(fun (x, typ) ->
            populate_index ~current_type_ast_level:new_typ_level
              ~current_typevars:new_typvars typ
            >>= fun typ -> Ok (x, typ))
        |> Or_error.combine_errors
        >>= fun ctx ->
        populate_index ~current_type_ast_level:new_typ_level
          ~current_typevars:new_typvars t
        >>= fun t -> Ok (TBox (tvctx, ctx, t))
    | TProd tlist ->
        List.map tlist
          ~f:(populate_index ~current_type_ast_level ~current_typevars)
        |> Or_error.combine_errors
        >>= fun tlist -> Ok (TProd tlist)
    | TSum (t1, t2) ->
        populate_index ~current_type_ast_level ~current_typevars t1
        >>= fun t1 ->
        populate_index ~current_type_ast_level ~current_typevars t2
        >>= fun t2 -> Ok (TSum (t1, t2))
    | TRef t ->
        populate_index ~current_type_ast_level ~current_typevars t >>= fun t ->
        Ok (TRef t)
    | TArray t ->
        populate_index ~current_type_ast_level ~current_typevars t >>= fun t ->
        Ok (TArray t)
    | TVar v ->
        TypeVar.populate_index v ~current_level:current_type_ast_level
          ~current_identifiers:current_typevars
        >>= fun v -> Ok (TVar v)
    | TForall (v, typ) ->
        let new_typevars =
          String.Map.set current_typevars ~key:(TypeVar.get_name v)
            ~data:current_type_ast_level
        in
        populate_index
          ~current_type_ast_level:(current_type_ast_level + 1)
          ~current_typevars:new_typevars typ
        >>= fun typ -> Ok (TForall (v, typ))

  let rec shift_indices typ ~type_depth ~type_offset =
    let open Or_error.Monad_infix in
    match typ with
    | TUnit -> Ok TUnit
    | TBool -> Ok TBool
    | TInt -> Ok TInt
    | TChar -> Ok TChar
    | TString -> Ok TString
    | TIdentifier (tlist, id) ->
        List.map ~f:(shift_indices ~type_depth ~type_offset) tlist
        |> Or_error.combine_errors
        >>= fun tlist -> Ok (TIdentifier (tlist, id))
        (*TODO: modify here for polymorphic ADTs*)
    | TFun (t1, t2) ->
        shift_indices ~type_depth ~type_offset t1 >>= fun t1 ->
        shift_indices ~type_depth ~type_offset t2 >>= fun t2 ->
        Ok (TFun (t1, t2))
    | TBox (tvctx, ctx, t) ->
        let new_type_depth =
          if TypeVarContext.is_empty tvctx then type_depth else type_depth + 1
        in
        List.map ctx ~f:(fun (x, typ) ->
            shift_indices ~type_depth:new_type_depth ~type_offset typ
            >>= fun typ -> Ok (x, typ))
        |> Or_error.combine_errors
        >>= fun ctx ->
        shift_indices ~type_depth:new_type_depth ~type_offset t >>= fun t ->
        Ok (TBox (tvctx, ctx, t))
    | TProd tlist ->
        List.map tlist ~f:(shift_indices ~type_depth ~type_offset)
        |> Or_error.combine_errors
        >>= fun tlist -> Ok (TProd tlist)
    | TSum (t1, t2) ->
        shift_indices ~type_depth ~type_offset t1 >>= fun t1 ->
        shift_indices ~type_depth ~type_offset t2 >>= fun t2 ->
        Ok (TSum (t1, t2))
    | TRef t ->
        shift_indices ~type_depth ~type_offset t >>= fun t -> Ok (TRef t)
    | TArray t ->
        shift_indices ~type_depth ~type_offset t >>= fun t -> Ok (TArray t)
    | TVar v ->
        TypeVar.shift v ~depth:type_depth ~offset:type_offset >>= fun v ->
        Ok (TVar v)
    | TForall (v, typ) ->
        shift_indices ~type_depth:(type_depth + 1) ~type_offset typ
        >>= fun typ -> Ok (TForall (v, typ))
end

and IdentifierDefn : sig
  type t = ObjIdentifier.t * Typ.t [@@deriving sexp, show, compare, equal]

  val of_past : Past.IdentifierDefn.t -> t

  val populate_index :
    t ->
    current_type_ast_level:int ->
    current_typevars:int String.Map.t ->
    t Or_error.t

  val shift_indices : t -> type_depth:int -> type_offset:int -> t Or_error.t
end = struct
  type t = ObjIdentifier.t * Typ.t [@@deriving sexp, show, compare, equal]

  let of_past (id, t1) = (ObjIdentifier.of_past id, Typ.of_past t1)

  let populate_index (id, typ) ~current_type_ast_level ~current_typevars =
    let open Or_error.Monad_infix in
    Typ.populate_index typ ~current_type_ast_level ~current_typevars
    >>= fun typ -> Ok (id, typ)

  let shift_indices (id, typ) ~type_depth ~type_offset =
    let open Or_error.Monad_infix in
    Typ.shift_indices typ ~type_depth ~type_offset >>= fun typ -> Ok (id, typ)
end

and RefCell : sig
  type t [@@deriving sexp, show, equal, compare]

  val get_id : t -> int64
  val of_value : Value.t -> t
  val deref : t -> Value.t
  val assign : t -> Value.t -> unit

  (*Aliases*)
  val ref : Value.t -> t
  val ( ! ) : t -> Value.t
  val ( := ) : t -> Value.t -> unit
end = struct
  type t = int64 * (Value.t ref[@compare.ignore] [@equal.ignore])
  [@@deriving sexp, show, equal, compare]

  let counter : int64 ref = ref Int64.zero
  let get_id (id, _) = id

  let of_value v =
    let c = !counter in
    counter := Int64.succ !counter;
    (c, ref v)

  let deref (_, r) = !r
  let assign (_, r) v = r := v
  let ref = of_value
  let ( ! ) = deref
  let ( := ) = assign
end

and ArrayCell : sig
  type t [@@deriving sexp, show, equal, compare]

  val get_id : t -> int64
  val of_list : Value.t list -> t
  val get : t -> int -> Value.t
  val set : t -> int -> Value.t -> unit
  val to_list : t -> Value.t list
  val length : t -> int
end = struct
  type t = int64 * (Value.t array[@compare.ignore] [@equal.ignore])
  [@@deriving sexp, show, equal, compare]

  let counter : int64 ref = ref Int64.zero
  let get_id (id, _) = id

  let of_list vs =
    let c = !counter in
    counter := Int64.succ !counter;
    (c, Array.of_list vs)

  let get (_, arr) i = arr.(i)
  let set (_, arr) i v = arr.(i) <- v
  let to_list (_, arr) = Array.to_list arr
  let length (_, arr) = Array.length arr
end

and Context : sig
  type t = IdentifierDefn.t list [@@deriving sexp, show, compare, equal]

  val of_past : Past.Context.t -> t

  val populate_index :
    t ->
    current_type_ast_level:int ->
    current_typevars:int String.Map.t ->
    t Or_error.t

  val shift_indices : t -> type_depth:int -> type_offset:int -> t Or_error.t
  val contains_duplicate_ids : t -> bool
end = struct
  type t = IdentifierDefn.t list [@@deriving sexp, show, compare, equal]

  let of_past ident_defn_list =
    List.map ident_defn_list ~f:IdentifierDefn.of_past

  let populate_index v ~current_type_ast_level ~current_typevars =
    List.map v
      ~f:
        (IdentifierDefn.populate_index ~current_type_ast_level ~current_typevars)
    |> Or_error.combine_errors

  let shift_indices v ~type_depth ~type_offset =
    List.map v ~f:(IdentifierDefn.shift_indices ~type_depth ~type_offset)
    |> Or_error.combine_errors

  let contains_duplicate_ids xs =
    List.contains_dup xs ~compare:(fun (id1, _) (id2, _) ->
        String.compare (ObjIdentifier.get_name id1) (ObjIdentifier.get_name id2))
end

and TypeVarContext : sig
  type t = TypeVar.t list [@@deriving sexp, show, equal, compare]

  val of_past : Past.TypeVarContext.t -> t
  val is_empty : t -> bool
  val contains_duplicates : t -> bool
end = struct
  type t = TypeVar.t list [@@deriving sexp, show, equal, compare]

  let of_past = List.map ~f:TypeVar.of_past
  let is_empty = List.is_empty
  let contains_duplicates xs = List.contains_dup xs ~compare:TypeVar.compare
end

and BinaryOperator : sig
  type t =
    | ADD
    | SUB
    | MUL
    | DIV
    | MOD
    | EQ
    | NEQ
    | GTE
    | GT
    | LTE
    | LT
    | AND
    | OR
    | CHARSTRINGCONCAT
    | STRINGCONCAT
    | ASSIGN
    | SEQ
    | ARRAY_INDEX
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.BinaryOperator.t -> t
end = struct
  type t =
    | ADD
    | SUB
    | MUL
    | DIV
    | MOD
    | EQ
    | NEQ
    | GTE
    | GT
    | LTE
    | LT
    | AND
    | OR
    | CHARSTRINGCONCAT
    | STRINGCONCAT
    | ASSIGN
    | SEQ
    | ARRAY_INDEX
  [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.BinaryOperator.ADD -> ADD
    | Past.BinaryOperator.SUB -> SUB
    | Past.BinaryOperator.MUL -> MUL
    | Past.BinaryOperator.DIV -> DIV
    | Past.BinaryOperator.MOD -> MOD
    | Past.BinaryOperator.EQ -> EQ
    | Past.BinaryOperator.NEQ -> NEQ
    | Past.BinaryOperator.GTE -> GTE
    | Past.BinaryOperator.GT -> GT
    | Past.BinaryOperator.LTE -> LTE
    | Past.BinaryOperator.LT -> LT
    | Past.BinaryOperator.AND -> AND
    | Past.BinaryOperator.OR -> OR
    | Past.BinaryOperator.CHARSTRINGCONCAT -> CHARSTRINGCONCAT
    | Past.BinaryOperator.STRINGCONCAT -> STRINGCONCAT
    | Past.BinaryOperator.ASSIGN -> ASSIGN
    | Past.BinaryOperator.SEQ -> SEQ
    | Past.BinaryOperator.ARRAY_INDEX -> ARRAY_INDEX
end

and UnaryOperator : sig
  type t = NEG | NOT | DEREF | ARRAY_LEN
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.UnaryOperator.t -> t
end = struct
  type t = NEG | NOT | DEREF | ARRAY_LEN
  [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.UnaryOperator.NEG -> NEG
    | Past.UnaryOperator.NOT -> NOT
    | Past.UnaryOperator.DEREF -> DEREF
    | Past.UnaryOperator.ARRAY_LEN -> ARRAY_LEN
end

and Constant : sig
  type t =
    | Integer of int
    | Boolean of bool
    | Unit
    | Character of char
    | String of string
    | Reference of RefCell.t
    | Array of ArrayCell.t
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.Constant.t -> t
end = struct
  type t =
    | Integer of int
    | Boolean of bool
    | Unit
    | Character of char
    | String of string
    | Reference of RefCell.t
    | Array of ArrayCell.t
  [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.Constant.Integer i -> Integer i
    | Past.Constant.Boolean b -> Boolean b
    | Past.Constant.Unit -> Unit
    | Past.Constant.Character c -> Character c
    | Past.Constant.String s -> String s
end

and Pattern : sig
  type t =
    | Datatype of (Constructor.t * ObjIdentifier.t list)
      (*Empty list means that data type doesn't have arguments.*)
    | Inl of ObjIdentifier.t
    | Inr of ObjIdentifier.t
    | Prod of ObjIdentifier.t list
    | Id of ObjIdentifier.t
    | Wildcard
    | String of string
    | ConcatCharString of ObjIdentifier.t * ObjIdentifier.t
  [@@deriving sexp, show, equal, compare]

  val of_past : Past.Pattern.t -> t
  val get_binders : t -> ObjIdentifier.t list
end = struct
  type t =
    | Datatype of (Constructor.t * ObjIdentifier.t list)
      (*Empty list means that data type doesn't have arguments.*)
    | Inl of ObjIdentifier.t
    | Inr of ObjIdentifier.t
    | Prod of ObjIdentifier.t list
    | Id of ObjIdentifier.t
    | Wildcard
    | String of string
    | ConcatCharString of ObjIdentifier.t * ObjIdentifier.t
  [@@deriving sexp, show, equal, compare]

  let of_past = function
    | Past.Pattern.Datatype (cons, objid_list) ->
        Datatype
          ( Constructor.of_past cons,
            List.map objid_list ~f:ObjIdentifier.of_past )
    | Past.Pattern.Inl id -> Inl (ObjIdentifier.of_past id)
    | Past.Pattern.Inr id -> Inr (ObjIdentifier.of_past id)
    | Past.Pattern.Prod id_list ->
        Prod (List.map id_list ~f:ObjIdentifier.of_past)
    | Past.Pattern.Id id -> Id (ObjIdentifier.of_past id)
    | Past.Pattern.Wildcard -> Wildcard
    | Past.Pattern.String s -> String s
    | Past.Pattern.ConcatCharString (c, s) ->
        ConcatCharString (ObjIdentifier.of_past c, ObjIdentifier.of_past s)

  let get_binders = function
    | Datatype (_, objid_list) -> objid_list
    | Inl id -> [ id ]
    | Inr id -> [ id ]
    | Prod id_list -> id_list
    | Id id -> [ id ]
    | Wildcard -> []
    | String _ -> []
    | ConcatCharString (c, s) -> [ c; s ]
end

and Expr : sig
  type t =
    | Identifier of ObjIdentifier.t (*x*)
    | Constant of Constant.t (*c*)
    | UnaryOp of UnaryOperator.t * t (*unop e*)
    | BinaryOp of BinaryOperator.t * t * t (*e op e'*)
    | Prod of t list (*(e, e')*)
    (* | Fst of t (*fst e*)
       | Snd of t snd e *)
    | Nth of (t * int)
    | Left of Typ.t * Typ.t * t (*L[A,B] e*)
    | Right of Typ.t * Typ.t * t (*R[A,B] e*)
    | Case of t * IdentifierDefn.t * t * IdentifierDefn.t * t
      (*case e of
        L (x: A) -> e' | R (y: B) -> e'' translates to 1 expr and 2 lambdas*)
    | Lambda of IdentifierDefn.t * t (*fun (x : A) -> e*)
    | Application of t * t (*e e'*)
    | IfThenElse of t * t * t (*if e then e' else e''*)
    | LetBinding of IdentifierDefn.t * t * t (*let x: A = e in e'*)
    | LetRec of IdentifierDefn.t * t * t
    (*let rec f: A->B =
      e[f] in e'*)
    | LetRecMutual of (IdentifierDefn.t * t) list * t
    | Box of TypeVarContext.t * Context.t * t (*box (x:A, y:B |- e)*)
    | LetBox of MetaIdentifier.t * t * t (*let box u = e in e'*)
    | Closure of
        MetaIdentifier.t * Typ.t list * t list (*u with (e1, e2, e3, ...)*)
    | Constr of Constructor.t * Typ.t list * t option (* Constr e*)
    | Match of t * (Pattern.t * t) list
    | Lift of Typ.t * t
    | EValue of Value.t
    | Ref of t
    | While of t * t
    | Array of t list
    | ArrayAssign of t * t * t
    | BigLambda of TypeVar.t * t
    | TypeApply of t * Typ.t
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.Expr.t -> t

  val populate_index :
    t ->
    current_ast_level:int ->
    current_identifiers:int String.Map.t ->
    current_meta_ast_level:int ->
    current_meta_identifiers:int String.Map.t ->
    current_type_ast_level:int ->
    current_typevars:int String.Map.t ->
    t Or_error.t

  val shift_indices :
    t ->
    obj_depth:int ->
    meta_depth:int ->
    type_depth:int ->
    obj_offset:int ->
    meta_offset:int ->
    type_offset:int ->
    t Or_error.t

  val to_val : t -> Value.t option
end = struct
  type t =
    | Identifier of ObjIdentifier.t (*x*)
    | Constant of Constant.t (*c*)
    | UnaryOp of UnaryOperator.t * t (*unop e*)
    | BinaryOp of BinaryOperator.t * t * t (*e op e'*)
    | Prod of t list (*(e, e')*)
    (* | Fst of t (*fst e*)
       | Snd of t snd e *)
    | Nth of (t * int)
    | Left of Typ.t * Typ.t * t (*L[A,B] e*)
    | Right of Typ.t * Typ.t * t (*R[A,B] e*)
    | Case of t * IdentifierDefn.t * t * IdentifierDefn.t * t
      (*match e with
        L (x: A) -> e' | R (y: B) -> e'' translates to 1 expr and 2 lambdas*)
    | Lambda of IdentifierDefn.t * t (*fun (x : A) -> e*)
    | Application of t * t (*e e'*)
    | IfThenElse of t * t * t (*if e then e' else e''*)
    | LetBinding of IdentifierDefn.t * t * t (*let x: A = e in e'*)
    | LetRec of IdentifierDefn.t * t * t
    (*let rec f: A->B =
      e[f] in e'*)
    | LetRecMutual of (IdentifierDefn.t * t) list * t
    | Box of TypeVarContext.t * Context.t * t (*box (x:A, y:B |- e)*)
    | LetBox of MetaIdentifier.t * t * t (*let box u = e in e'*)
    | Closure of
        MetaIdentifier.t * Typ.t list * t list (*u with (e1, e2, e3, ...)*)
    | Constr of Constructor.t * Typ.t list * t option (* Constr e*)
    | Match of t * (Pattern.t * t) list
    | Lift of Typ.t * t
    | EValue of Value.t
    | Ref of t
    | While of t * t
    | Array of t list
    | ArrayAssign of t * t * t
    | BigLambda of TypeVar.t * t
    | TypeApply of t * Typ.t
  [@@deriving sexp, show, compare, equal]

  (* TODO: let rec pp_to_code expr = () *)

  let rec of_past = function
    | Past.Expr.Identifier id -> Identifier (ObjIdentifier.of_past id)
    | Past.Expr.Constant c -> Constant (Constant.of_past c)
    | Past.Expr.UnaryOp (op, expr) ->
        UnaryOp (UnaryOperator.of_past op, of_past expr)
    | Past.Expr.BinaryOp (op, expr, expr2) ->
        BinaryOp (BinaryOperator.of_past op, of_past expr, of_past expr2)
    | Past.Expr.Prod expr_list -> Prod (List.map expr_list ~f:of_past)
    (* | Past.Expr.Fst expr -> Fst (of_past expr)
       | Past.Expr.Snd expr -> Snd (of_past expr) *)
    | Past.Expr.Nth (expr, i) -> Nth (of_past expr, i)
    | Past.Expr.Left (t1, t2, expr) ->
        Left (Typ.of_past t1, Typ.of_past t2, of_past expr)
    | Past.Expr.Right (t1, t2, expr) ->
        Right (Typ.of_past t1, Typ.of_past t2, of_past expr)
    | Past.Expr.Case (e, iddef1, e1, iddef2, e2) ->
        Case
          ( of_past e,
            IdentifierDefn.of_past iddef1,
            of_past e1,
            IdentifierDefn.of_past iddef2,
            of_past e2 )
    | Past.Expr.Lambda (iddef, e) ->
        Lambda (IdentifierDefn.of_past iddef, of_past e)
    | Past.Expr.Application (e1, e2) -> Application (of_past e1, of_past e2)
    | Past.Expr.IfThenElse (b, e1, e2) ->
        IfThenElse (of_past b, of_past e1, of_past e2)
    | Past.Expr.LetBinding (iddef, e, e2) ->
        LetBinding (IdentifierDefn.of_past iddef, of_past e, of_past e2)
    | Past.Expr.LetRec (iddef, e, e2) ->
        LetRec (IdentifierDefn.of_past iddef, of_past e, of_past e2)
    | Past.Expr.LetRecMutual (iddef_e_list, e2) ->
        LetRecMutual
          ( List.map iddef_e_list ~f:(fun (iddef, e) ->
                (IdentifierDefn.of_past iddef, of_past e)),
            of_past e2 )
    | Past.Expr.Box (tvctx, ctx, e) ->
        Box (TypeVarContext.of_past tvctx, Context.of_past ctx, of_past e)
    | Past.Expr.LetBox (metaid, e, e2) ->
        LetBox (MetaIdentifier.of_past metaid, of_past e, of_past e2)
    | Past.Expr.Closure (metaid, typs, exprs) ->
        Closure
          ( MetaIdentifier.of_past metaid,
            List.map typs ~f:Typ.of_past,
            List.map exprs ~f:of_past )
    | Past.Expr.Constr (constructor, tlist, Some value) ->
        (* TODO: MODIFY *)
        Constr
          ( Constructor.of_past constructor,
            List.map ~f:Typ.of_past tlist,
            Some (Expr.of_past value) )
    | Past.Expr.Constr (constructor, tlist, None) ->
        Constr
          (Constructor.of_past constructor, List.map ~f:Typ.of_past tlist, None)
        (* TODO: MODIFY *)
    | Past.Expr.Match (e, pattns) ->
        Match
          ( of_past e,
            List.map pattns ~f:(fun (pattn, expr) ->
                (Pattern.of_past pattn, of_past expr)) )
    | Past.Expr.Lift (typ, expr) -> Lift (Typ.of_past typ, of_past expr)
    | Past.Expr.Ref e -> Ref (of_past e)
    | Past.Expr.While (p, e) -> While (of_past p, of_past e)
    | Past.Expr.Array es -> Array (List.map es ~f:of_past)
    | Past.Expr.ArrayAssign (arr, index, assign_to) ->
        ArrayAssign (of_past arr, of_past index, of_past assign_to)
    | Past.Expr.BigLambda (v, e) -> BigLambda (TypeVar.of_past v, of_past e)
    | Past.Expr.TypeApply (e, t) -> TypeApply (of_past e, Typ.of_past t)

  let rec populate_index expr ~current_ast_level ~current_identifiers
      ~current_meta_ast_level ~current_meta_identifiers ~current_type_ast_level
      ~current_typevars =
    let open Or_error.Monad_infix in
    match expr with
    | Identifier id ->
        ObjIdentifier.populate_index id ~current_level:current_ast_level
          ~current_identifiers
        >>= fun id -> Ok (Identifier id)
        (*Addition for De Bruijn*)
    | Constant c -> Ok (Constant c)
    | UnaryOp (op, expr) ->
        populate_index expr ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun expr -> Ok (UnaryOp (op, expr))
    | BinaryOp (op, expr, expr2) ->
        populate_index expr ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun expr ->
        populate_index expr2 ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun expr2 -> Ok (BinaryOp (op, expr, expr2))
    | Prod exprs ->
        List.map exprs
          ~f:
            (populate_index ~current_ast_level ~current_identifiers
               ~current_meta_ast_level ~current_meta_identifiers
               ~current_type_ast_level ~current_typevars)
        |> Or_error.combine_errors
        >>= fun new_exprs -> Ok (Prod new_exprs)
    | Nth (expr, i) ->
        populate_index expr ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun expr -> Ok (Nth (expr, i))
    | Left (t1, t2, expr) ->
        Typ.populate_index ~current_type_ast_level ~current_typevars t1
        >>= fun t1 ->
        Typ.populate_index ~current_type_ast_level ~current_typevars t2
        >>= fun t2 ->
        populate_index expr ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun expr -> Ok (Left (t1, t2, expr))
    | Right (t1, t2, expr) ->
        Typ.populate_index ~current_type_ast_level ~current_typevars t1
        >>= fun t1 ->
        Typ.populate_index ~current_type_ast_level ~current_typevars t2
        >>= fun t2 ->
        populate_index expr ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun expr -> Ok (Right (t1, t2, expr))
    | Case (e, iddef1, e1, iddef2, e2) ->
        (*Addition for De Bruijn*)
        IdentifierDefn.populate_index ~current_type_ast_level ~current_typevars
          iddef1
        >>= fun iddef1 ->
        IdentifierDefn.populate_index ~current_type_ast_level ~current_typevars
          iddef2
        >>= fun iddef2 ->
        let id1, _ = iddef1 and id2, _ = iddef2 in

        let new_current_identifiers_1 =
          String.Map.set current_identifiers
            ~key:(ObjIdentifier.get_name id1)
            ~data:current_ast_level
        in
        let new_current_identifiers_2 =
          String.Map.set current_identifiers
            ~key:(ObjIdentifier.get_name id2)
            ~data:current_ast_level
        in
        let new_level = current_ast_level + 1 in
        populate_index e ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun e ->
        populate_index e1 ~current_ast_level:new_level
          ~current_identifiers:new_current_identifiers_1 ~current_meta_ast_level
          ~current_meta_identifiers ~current_type_ast_level ~current_typevars
        >>= fun e1 ->
        populate_index e2 ~current_ast_level:new_level
          ~current_identifiers:new_current_identifiers_2 ~current_meta_ast_level
          ~current_meta_identifiers ~current_type_ast_level ~current_typevars
        >>= fun e2 ->
        (*We don't need to modify iddefs because the identifiers inside are already having the right index, i.e. None*)
        Ok (Case (e, iddef1, e1, iddef2, e2))
    | Lambda (iddef, e) ->
        IdentifierDefn.populate_index ~current_type_ast_level ~current_typevars
          iddef
        >>= fun iddef ->
        (*Addition for De Bruijn*)
        let id, _ = iddef in
        let new_current_identifiers =
          String.Map.set current_identifiers
            ~key:(ObjIdentifier.get_name id)
            ~data:current_ast_level
        in
        let new_level = current_ast_level + 1 in
        populate_index e ~current_ast_level:new_level
          ~current_identifiers:new_current_identifiers ~current_meta_ast_level
          ~current_meta_identifiers ~current_type_ast_level ~current_typevars
        >>= fun e -> Ok (Lambda (iddef, e))
    | Application (e1, e2) ->
        populate_index e1 ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun e1 ->
        populate_index e2 ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun e2 -> Ok (Application (e1, e2))
    | IfThenElse (b, e1, e2) ->
        populate_index b ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun b ->
        populate_index e1 ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun e1 ->
        populate_index e2 ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun e2 -> Ok (IfThenElse (b, e1, e2))
    | LetBinding (iddef, e, e2) ->
        IdentifierDefn.populate_index ~current_type_ast_level ~current_typevars
          iddef
        >>= fun iddef ->
        let id, _ = iddef in
        let new_current_identifiers =
          String.Map.set current_identifiers
            ~key:(ObjIdentifier.get_name id)
            ~data:current_ast_level
        in
        let new_level = current_ast_level + 1 in
        populate_index e ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun e ->
        populate_index e2 ~current_ast_level:new_level
          ~current_identifiers:new_current_identifiers ~current_meta_ast_level
          ~current_meta_identifiers ~current_type_ast_level ~current_typevars
        >>= fun e2 -> Ok (LetBinding (iddef, e, e2))
    | LetRec (iddef, e, e2) ->
        IdentifierDefn.populate_index ~current_type_ast_level ~current_typevars
          iddef
        >>= fun iddef ->
        (*Addition for De Bruijn indices:
          Importantly, we have let rec f (level n) = e (level n+1) in e'(level n+1)*)
        let id, _ = iddef in
        let new_level = current_ast_level + 1 in
        let new_current_identifiers =
          String.Map.set current_identifiers
            ~key:(ObjIdentifier.get_name id)
            ~data:current_ast_level
        in
        populate_index e ~current_ast_level:new_level
          ~current_identifiers:new_current_identifiers ~current_meta_ast_level
          ~current_meta_identifiers ~current_type_ast_level ~current_typevars
        >>= fun e ->
        populate_index e2 ~current_ast_level:new_level
          ~current_identifiers:new_current_identifiers ~current_meta_ast_level
          ~current_meta_identifiers ~current_type_ast_level ~current_typevars
        >>= fun e2 -> Ok (LetRec (iddef, e, e2))
    | LetRecMutual (iddef_e_list, e2) ->
        (*First add all the defns in the current identifiers, at current level*)
        let new_current_identifiers =
          List.fold iddef_e_list ~init:current_identifiers
            ~f:(fun acc ((id, _), _) ->
              String.Map.set acc
                ~key:(ObjIdentifier.get_name id)
                ~data:current_ast_level)
        in
        let new_level = current_ast_level + 1 in
        List.map iddef_e_list ~f:(fun (iddef, e) ->
            IdentifierDefn.populate_index ~current_type_ast_level
              ~current_typevars iddef
            >>= fun iddef ->
            populate_index e ~current_ast_level:new_level
              ~current_identifiers:new_current_identifiers
              ~current_meta_ast_level ~current_meta_identifiers
              ~current_type_ast_level ~current_typevars
            >>= fun e -> Ok (iddef, e))
        |> Or_error.combine_errors
        >>= fun iddef_e_list ->
        populate_index e2 ~current_ast_level:new_level
          ~current_identifiers:new_current_identifiers ~current_meta_ast_level
          ~current_meta_identifiers ~current_type_ast_level ~current_typevars
        >>= fun e2 -> Ok (LetRecMutual (iddef_e_list, e2))
    | Box (tvctx, ctx, e) ->
        (*
          Explanation: ctx is a list of iddefn so no need to populate indices; we create a fresh context with nothing in it like for a closure, and
          base all our De Bruijn indices from that, starting from 0, ignoring whatever is outside completely.
          However, the box still has access to meta variables from outside; just not object variables. TODO: Generalise

          New: context needs populating type indices
        *)
        (* TODO: If polymorphic,  i.e. box tvctx is non empty, handle them first. *)
        let new_typ_level, new_typvars =
          if TypeVarContext.is_empty tvctx then
            (* If empty then don't add a level. *)
            (current_type_ast_level, current_typevars)
          else
            (* Insert context and add level *)
            ( current_type_ast_level + 1,
              List.fold tvctx ~init:current_typevars ~f:(fun acc tv ->
                  String.Map.set acc ~key:(TypeVar.get_name tv)
                    ~data:current_type_ast_level) )
        in
        Context.populate_index ~current_type_ast_level:new_typ_level
          ~current_typevars:new_typvars ctx
        >>= fun ctx ->
        Or_error.tag
          ~tag:
            "DeBruijnPopulationError[OBJECT]: There are duplicated identifier \
             definitions in the box context."
          (String.Map.of_alist_or_error
             (List.map ctx ~f:(fun (id, _) -> (ObjIdentifier.get_name id, 0))))
        >>= fun new_current_identifiers ->
        (* Actually we need to populate the types *)
        populate_index e ~current_ast_level:1
          ~current_identifiers:new_current_identifiers ~current_meta_ast_level
          ~current_meta_identifiers ~current_type_ast_level:new_typ_level
          ~current_typevars:new_typvars
        >>= fun e -> Ok (Box (tvctx, ctx, e))
    | LetBox (metaid, e, e2) ->
        let new_current_meta_identifiers =
          String.Map.set current_meta_identifiers
            ~key:(MetaIdentifier.get_name metaid)
            ~data:current_meta_ast_level
        in
        let new_meta_ast_level = current_meta_ast_level + 1 in
        populate_index e ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun e ->
        populate_index e2 ~current_ast_level ~current_identifiers
          ~current_meta_ast_level:new_meta_ast_level
          ~current_meta_identifiers:new_current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun e2 -> Ok (LetBox (metaid, e, e2))
    | Closure (metaid, typs, exprs) ->
        List.map typs
          ~f:(Typ.populate_index ~current_type_ast_level ~current_typevars)
        |> Or_error.combine_errors
        |> Or_error.tag
             ~tag:
               "DeBruijnPopulationError: Error in typs\n\
               \    in the explicit substitution"
        >>= fun typs ->
        let populated_expr_or_error_list =
          List.map exprs ~f:(fun e ->
              populate_index e ~current_ast_level ~current_identifiers
                ~current_meta_ast_level ~current_meta_identifiers
                ~current_type_ast_level ~current_typevars)
        in
        Or_error.tag
          ~tag:
            "DeBruijnPopulationError[OBJECT]: Error in explicit substitution \
             of a closure"
          (Or_error.combine_errors populated_expr_or_error_list)
        >>= fun exprs ->
        MetaIdentifier.populate_index metaid
          ~current_level:current_meta_ast_level
          ~current_identifiers:current_meta_identifiers
        >>= fun metaid -> Ok (Closure (metaid, typs, exprs))
    | Constr (tid, tlist, e_opt) -> (
        (* Or_error.unimplemented "not implemented" *)
        (* Congruence*)
        List.map
          ~f:(Typ.populate_index ~current_type_ast_level ~current_typevars)
          tlist
        |> Or_error.combine_errors
        >>= fun tlist ->
        match e_opt with
        | None -> Ok (Constr (tid, tlist, None))
        | Some e ->
            populate_index ~current_ast_level ~current_identifiers
              ~current_meta_ast_level ~current_meta_identifiers
              ~current_type_ast_level ~current_typevars e
            >>= fun e -> Ok (Constr (tid, tlist, Some e)))
    | Match (e, pattn_expr_list) ->
        (* Or_error.unimplemented "Not implemented" *)
        (* Get all binders, and if there are binders, increment level, otherwise don't *)
        populate_index e ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars
        >>= fun e ->
        let process_binder_expr (pattern, expr) =
          let binders = Pattern.get_binders pattern in
          if List.is_empty binders then
            (* No adding new binders and no incrementing *)
            populate_index ~current_ast_level ~current_identifiers
              ~current_meta_ast_level ~current_meta_identifiers
              ~current_type_ast_level ~current_typevars expr
            >>= fun expr -> Ok (pattern, expr)
          else
            (* Add the binders in obj context *)
            let new_current_identifiers =
              List.fold binders ~init:current_identifiers ~f:(fun acc binder ->
                  String.Map.set acc
                    ~key:(ObjIdentifier.get_name binder)
                    ~data:current_ast_level)
            in
            (* And populate indices with incremented context *)
            let new_level = current_ast_level + 1 in
            populate_index expr ~current_ast_level:new_level
              ~current_identifiers:new_current_identifiers
              ~current_meta_ast_level ~current_meta_identifiers
              ~current_type_ast_level ~current_typevars
            >>= fun expr -> Ok (pattern, expr)
        in
        List.map pattn_expr_list ~f:process_binder_expr
        |> Or_error.combine_errors
        |> Or_error.tag
             ~tag:"DeBruijnPopulationError[OBJECT]: Error in Match statement."
        >>= fun new_pattn_expr_list -> Ok (Match (e, new_pattn_expr_list))
    | Lift (typ, expr) ->
        Typ.populate_index typ ~current_type_ast_level ~current_typevars
        >>= fun typ ->
        populate_index ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars expr
        >>= fun expr -> Ok (Lift (typ, expr))
    | EValue v ->
        Value.to_expr v
        |> populate_index ~current_ast_level ~current_identifiers
             ~current_meta_ast_level ~current_meta_identifiers
             ~current_type_ast_level ~current_typevars
    | Ref e ->
        populate_index ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars e
        >>= fun expr -> Ok (Ref expr)
    | While (p, e) ->
        populate_index ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars p
        >>= fun p ->
        populate_index ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars e
        >>= fun e -> Ok (While (p, e))
    | Array es ->
        List.map es
          ~f:
            (populate_index ~current_ast_level ~current_identifiers
               ~current_meta_ast_level ~current_meta_identifiers
               ~current_type_ast_level ~current_typevars)
        |> Or_error.combine_errors
        >>= fun es -> Ok (Array es)
    | ArrayAssign (e1, e2, e3) ->
        populate_index ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars e1
        >>= fun e1 ->
        populate_index ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars e2
        >>= fun e2 ->
        populate_index ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars e3
        >>= fun e3 -> Ok (ArrayAssign (e1, e2, e3))
    | BigLambda (v, e) ->
        let new_typevars =
          String.Map.set current_typevars ~key:(TypeVar.get_name v)
            ~data:current_type_ast_level
        in
        populate_index ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level:(current_type_ast_level + 1)
          ~current_typevars:new_typevars e
        >>= fun e -> Ok (BigLambda (v, e))
    | TypeApply (e, t) ->
        Typ.populate_index t ~current_type_ast_level ~current_typevars
        >>= fun t ->
        populate_index ~current_ast_level ~current_identifiers
          ~current_meta_ast_level ~current_meta_identifiers
          ~current_type_ast_level ~current_typevars e
        >>= fun e -> Ok (TypeApply (e, t))

  let rec shift_indices expr ~obj_depth ~meta_depth ~type_depth ~obj_offset
      ~meta_offset ~type_offset =
    let open Or_error.Monad_infix in
    match expr with
    | Identifier id ->
        Or_error.tag
          ~tag:"DeBruijnShiftingError[OBJECT]: Shifting object index failed."
          (ObjIdentifier.shift id ~depth:obj_depth ~offset:obj_offset)
        >>= fun id -> Ok (Identifier id)
    | Constant c -> Ok (Constant c)
    | UnaryOp (op, expr) ->
        shift_indices expr ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun expr -> Ok (UnaryOp (op, expr))
    | BinaryOp (op, expr, expr2) ->
        shift_indices expr ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun expr ->
        shift_indices expr2 ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun expr2 -> Ok (BinaryOp (op, expr, expr2))
    | Prod exprs ->
        List.map exprs
          ~f:
            (shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
               ~meta_offset ~type_offset)
        |> Or_error.combine_errors
        >>= fun exprs -> Ok (Prod exprs)
    | Nth (expr, i) ->
        shift_indices expr ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun expr -> Ok (Nth (expr, i))
    | Left (t1, t2, expr) ->
        Typ.shift_indices t1 ~type_depth ~type_offset >>= fun t1 ->
        Typ.shift_indices t2 ~type_depth ~type_offset >>= fun t2 ->
        shift_indices expr ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun expr -> Ok (Left (t1, t2, expr))
    | Right (t1, t2, expr) ->
        Typ.shift_indices t1 ~type_depth ~type_offset >>= fun t1 ->
        Typ.shift_indices t2 ~type_depth ~type_offset >>= fun t2 ->
        shift_indices expr ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun expr -> Ok (Right (t1, t2, expr))
    | Case (e, iddef1, e1, iddef2, e2) ->
        shift_indices e ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun e ->
        IdentifierDefn.shift_indices iddef1 ~type_depth ~type_offset
        >>= fun iddef1 ->
        shift_indices e1 ~obj_depth:(obj_depth + 1) ~meta_depth ~type_depth
          ~obj_offset ~meta_offset ~type_offset
        >>= fun e1 ->
        IdentifierDefn.shift_indices iddef2 ~type_depth ~type_offset
        >>= fun iddef2 ->
        shift_indices e2 ~obj_depth:(obj_depth + 1) ~meta_depth ~type_depth
          ~obj_offset ~meta_offset ~type_offset
        >>= fun e2 -> Ok (Case (e, iddef1, e1, iddef2, e2))
    | Lambda (iddef, e) ->
        IdentifierDefn.shift_indices iddef ~type_depth ~type_offset
        >>= fun iddef ->
        shift_indices e ~obj_depth:(obj_depth + 1) ~meta_depth ~type_depth
          ~obj_offset ~meta_offset ~type_offset
        >>= fun e -> Ok (Lambda (iddef, e))
    | Application (e1, e2) ->
        shift_indices e1 ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun e1 ->
        shift_indices e2 ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun e2 -> Ok (Application (e1, e2))
    | IfThenElse (b, e1, e2) ->
        shift_indices b ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun b ->
        shift_indices e1 ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun e1 ->
        shift_indices e2 ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun e2 -> Ok (IfThenElse (b, e1, e2))
    | LetBinding (iddef, e, e2) ->
        IdentifierDefn.shift_indices iddef ~type_depth ~type_offset
        >>= fun iddef ->
        shift_indices e ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun e ->
        shift_indices e2 ~obj_depth:(obj_depth + 1) ~meta_depth ~type_depth
          ~obj_offset ~meta_offset ~type_offset
        >>= fun e2 -> Ok (LetBinding (iddef, e, e2))
    | LetRec (iddef, e, e2) ->
        IdentifierDefn.shift_indices iddef ~type_depth ~type_offset
        >>= fun iddef ->
        shift_indices e ~obj_depth:(obj_depth + 1) ~meta_depth ~type_depth
          ~obj_offset ~meta_offset ~type_offset
        >>= fun e ->
        shift_indices e2 ~obj_depth:(obj_depth + 1) ~meta_depth ~type_depth
          ~obj_offset ~meta_offset ~type_offset
        >>= fun e2 -> Ok (LetRec (iddef, e, e2))
    | LetRecMutual (iddef_e_list, e2) ->
        List.map iddef_e_list ~f:(fun (iddef, e) ->
            IdentifierDefn.shift_indices iddef ~type_depth ~type_offset
            >>= fun iddef ->
            shift_indices e ~obj_depth:(obj_depth + 1) ~meta_depth ~type_depth
              ~obj_offset ~meta_offset ~type_offset
            >>= fun e -> Ok (iddef, e))
        |> Or_error.combine_errors
        >>= fun iddef_e_list ->
        shift_indices e2 ~obj_depth:(obj_depth + 1) ~meta_depth ~type_depth
          ~obj_offset ~meta_offset ~type_offset
        >>= fun e2 -> Ok (LetRecMutual (iddef_e_list, e2))
    | Box (tvctx, ctx, e) ->
        (* Again, when shifting we want to record the new depth in the case the typevar context is non-empty. *)
        let new_type_depth =
          if TypeVarContext.is_empty tvctx then type_depth else type_depth + 1
        in
        (*Can only shift meta indices*)
        Context.shift_indices ctx ~type_depth:new_type_depth ~type_offset
        >>= fun ctx ->
        shift_indices e ~obj_depth:(obj_depth + 1) ~meta_depth
          ~type_depth:new_type_depth ~obj_offset ~meta_offset ~type_offset
        >>= fun e -> Ok (Box (tvctx, ctx, e))
    | LetBox (metaid, e, e2) ->
        shift_indices e ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun e ->
        shift_indices e2 ~obj_depth ~meta_depth:(meta_depth + 1) ~type_depth
          ~obj_offset ~meta_offset ~type_offset
        >>= fun e2 -> Ok (LetBox (metaid, e, e2))
    | Closure (metaid, typs, exprs) ->
        List.map typs ~f:(Typ.shift_indices ~type_depth ~type_offset)
        |> Or_error.combine_errors
        |> Or_error.tag
             ~tag:
               "DeBruijnShiftingError[OBJECT]: Error in the explicit type \
                substitution term of a closure"
        >>= fun typs ->
        let expr_or_error_list =
          List.map exprs ~f:(fun e ->
              shift_indices e ~obj_depth ~meta_depth ~type_depth ~obj_offset
                ~meta_offset ~type_offset)
        in
        Or_error.tag
          ~tag:
            "DeBruijnShiftingError[OBJECT]: Error in the explicit substitution \
             term of a closure"
          (Or_error.combine_errors expr_or_error_list)
        >>= fun exprs ->
        Or_error.tag
          ~tag:"DeBruijnShiftingError[META]: Shifting meta index failed."
          (MetaIdentifier.shift metaid ~depth:meta_depth ~offset:meta_offset)
        >>= fun metaid -> Ok (Closure (metaid, typs, exprs))
    | Constr (tid, tlist, e_opt) -> (
        List.map ~f:(Typ.shift_indices ~type_depth ~type_offset) tlist
        |> Or_error.combine_errors
        >>= fun tlist ->
        match e_opt with
        | None -> Ok (Constr (tid, tlist, None))
        | Some e ->
            shift_indices e ~obj_depth ~meta_depth ~type_depth ~obj_offset
              ~meta_offset ~type_offset
            >>= fun e -> Ok (Constr (tid, tlist, Some e)))
    | Match (e, pattn_expr_list) ->
        shift_indices e ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset
        >>= fun e ->
        List.map pattn_expr_list ~f:(fun (pattn, expr) ->
            let binders = Pattern.get_binders pattn in
            (* Add 1 to obj depth only if BINDING *)
            let new_obj_depth =
              if List.is_empty binders then obj_depth else obj_depth + 1
            in
            shift_indices expr ~obj_depth:new_obj_depth ~meta_depth ~type_depth
              ~obj_offset ~meta_offset ~type_offset
            >>= fun new_expr -> Ok (pattn, new_expr))
        |> Or_error.combine_errors
        |> Or_error.tag
             ~tag:"DeBruijnShiftingError[OBJECT]: Error in match statement."
        >>= fun pattn_expr_list -> Ok (Match (e, pattn_expr_list))
    | Lift (typ, expr) ->
        Typ.shift_indices ~type_depth ~type_offset typ >>= fun typ ->
        shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset expr
        >>= fun expr -> Ok (Lift (typ, expr))
    | EValue v ->
        Value.to_expr v
        |> shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
             ~meta_offset ~type_offset
    | Ref e ->
        shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset e
        >>= fun expr -> Ok (Ref expr)
    | While (p, e) ->
        shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset p
        >>= fun p ->
        shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset e
        >>= fun e -> Ok (While (p, e))
    | Array es ->
        List.map es
          ~f:
            (shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
               ~meta_offset ~type_offset)
        |> Or_error.combine_errors
        >>= fun es -> Ok (Array es)
    | ArrayAssign (e1, e2, e3) ->
        shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset e1
        >>= fun e1 ->
        shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset e2
        >>= fun e2 ->
        shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset e3
        >>= fun e3 -> Ok (ArrayAssign (e1, e2, e3))
    | BigLambda (v, e) ->
        shift_indices ~obj_depth ~meta_depth ~type_depth:(type_depth + 1)
          ~obj_offset ~meta_offset ~type_offset e
        >>= fun e -> Ok (BigLambda (v, e))
    | TypeApply (e, t) ->
        Typ.shift_indices ~type_depth ~type_offset t >>= fun t ->
        shift_indices ~obj_depth ~meta_depth ~type_depth ~obj_offset
          ~meta_offset ~type_offset e
        >>= fun e -> Ok (TypeApply (e, t))

  let rec to_val expr =
    let open Option.Monad_infix in
    match expr with
    | Constant c -> Some (Value.Constant c)
    | Prod exprs ->
        let rec convert_exprs = function
          | [] -> Some []
          | e :: es -> (
              match to_val e with
              | None -> None
              | Some v -> convert_exprs es >>= fun vs -> Some (v :: vs))
        in
        convert_exprs exprs >>= fun vs -> Some (Value.Prod vs)
    | Lambda (iddef, e) -> Some (Value.Lambda (iddef, e))
    | Box (tvctx, ctx, e) -> Some (Value.Box (tvctx, ctx, e))
    | Constr (tid, tlist, e_opt) -> (
        match e_opt with
        | None -> Some (Value.Constr (tid, tlist, None))
        | Some e ->
            to_val e >>= fun v -> Some (Value.Constr (tid, tlist, Some v)))
    | EValue v -> Some v
    | _ -> None
end

and Value : sig
  type t =
    | Constant of Constant.t (*c*)
    | Prod of t list (*(e, e')*)
    | Left of Typ.t * Typ.t * t (*L[A,B] e*)
    | Right of Typ.t * Typ.t * t (*R[A,B] e*)
    | Lambda of IdentifierDefn.t * Expr.t (*fun (x : A) -> e*)
    | Box of TypeVarContext.t * Context.t * Expr.t (*box (x:A, y:B |- e)*)
    | Constr of Constructor.t * Typ.t list * t option
    | BigLambda of TypeVar.t * Expr.t
  [@@deriving sexp, show, compare, equal]

  val to_expr : Value.t -> Expr.t
  val to_expr_intensional : Value.t -> Expr.t
end = struct
  type t =
    | Constant of Constant.t (*c*)
    | Prod of t list (*(e, e')*)
    | Left of Typ.t * Typ.t * t (*L[A,B] e*)
    | Right of Typ.t * Typ.t * t (*R[A,B] e*)
    | Lambda of IdentifierDefn.t * Expr.t (*fun (x : A) -> e*)
    | Box of TypeVarContext.t * Context.t * Expr.t (*box (x:A, y:B |- e)*)
    | Constr of Constructor.t * Typ.t list * t option
    | BigLambda of TypeVar.t * Expr.t
  [@@deriving sexp, show, compare, equal]

  let to_expr = function
    | Constant c -> Expr.Constant c
    | Prod xs -> Expr.Prod (List.map xs ~f:(fun x -> Expr.EValue x))
    | Left (t1, t2, v) -> Expr.Left (t1, t2, Expr.EValue v)
    | Right (t1, t2, v) -> Expr.Right (t1, t2, Expr.EValue v)
    | Lambda (iddef, expr) -> Expr.Lambda (iddef, expr)
    | Box (tvctx, ctx, expr) -> Expr.Box (tvctx, ctx, expr)
    | Constr (constr, tlist, Some v) ->
        Expr.Constr (constr, tlist, Some (Expr.EValue v))
    | Constr (constr, tlist, None) -> Expr.Constr (constr, tlist, None)
    | BigLambda (typvar, e) -> Expr.BigLambda (typvar, e)

  let rec to_expr_intensional = function
    (*This function does not preserve semantics for impure terms, but rather converts to an intensional representation*)
    (*Also, this function converts to an expression *completely*.*)
    | Constant (Constant.Reference r) ->
        (*Conversion to intensional representation*)
        let open RefCell in
        Expr.Ref (to_expr_intensional !r)
    | Constant (Constant.Array arr) ->
        Expr.Array (arr |> ArrayCell.to_list |> List.map ~f:to_expr_intensional)
    | Constant c -> Expr.Constant c
    | Prod xs -> Expr.Prod (List.map xs ~f:to_expr_intensional)
    | Left (t1, t2, v) -> Expr.Left (t1, t2, to_expr_intensional v)
    | Right (t1, t2, v) -> Expr.Right (t1, t2, to_expr_intensional v)
    | Lambda (iddef, expr) -> Expr.Lambda (iddef, expr)
    | Box (tvctx, ctx, expr) -> Expr.Box (tvctx, ctx, expr)
    | Constr (constr, tlist, Some v) ->
        Expr.Constr (constr, tlist, Some (to_expr_intensional v))
    | Constr (constr, tlist, None) -> Expr.Constr (constr, tlist, None)
    | BigLambda (typvar, e) -> Expr.BigLambda (typvar, e)
end

and Directive : sig
  type t = Reset | Env | Quit [@@deriving sexp, show, compare, equal]

  val of_past : Past.Directive.t -> t
end = struct
  type t = Reset | Env | Quit [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.Directive.Reset -> Reset
    | Past.Directive.Env -> Env
    | Past.Directive.Quit -> Quit
end

and TopLevelDefn : sig
  type t =
    | Definition of IdentifierDefn.t * Expr.t
    | RecursiveDefinition of IdentifierDefn.t * Expr.t
    | MutualRecursiveDefinition of (IdentifierDefn.t * Expr.t) list
    | Expression of Expr.t
    | Directive of Directive.t
    | DatatypeDecl of
        (TypeVarContext.t
        * TypeIdentifier.t
        * (Constructor.t * Typ.t option) list)
        list
  [@@deriving sexp, show, compare, equal]

  val of_past : Past.TopLevelDefn.t -> t
  val populate_index : t -> t Or_error.t
end = struct
  (*Note added type for defns (not useful for now but useful for when adding inference) and exprs for the REPL*)
  type t =
    | Definition of IdentifierDefn.t * Expr.t
    | RecursiveDefinition of IdentifierDefn.t * Expr.t
    | MutualRecursiveDefinition of (IdentifierDefn.t * Expr.t) list
    | Expression of Expr.t
    | Directive of Directive.t
    | DatatypeDecl of
        (TypeVarContext.t
        * TypeIdentifier.t
        * (Constructor.t * Typ.t option) list)
        list
  [@@deriving sexp, show, compare, equal]

  let of_past = function
    | Past.TopLevelDefn.Definition (iddef, e) ->
        Definition (IdentifierDefn.of_past iddef, Expr.of_past e)
    | Past.TopLevelDefn.RecursiveDefinition (iddef, e) ->
        RecursiveDefinition (IdentifierDefn.of_past iddef, Expr.of_past e)
    | Past.TopLevelDefn.MutualRecursiveDefinition iddef_e_list ->
        MutualRecursiveDefinition
          (List.map iddef_e_list ~f:(fun (iddef, e) ->
               (IdentifierDefn.of_past iddef, Expr.of_past e)))
    | Past.TopLevelDefn.Expression e -> Expression (Expr.of_past e)
    | Past.TopLevelDefn.Directive d -> Directive (Directive.of_past d)
    | Past.TopLevelDefn.DatatypeDecl id_constr_typ_list_list ->
        let new_id_contr_typ_list_list =
          List.map id_constr_typ_list_list
            ~f:(fun (tvctx, id, constr_typ_list) ->
              (* TODO: PolyADT MODIFY *)
              ( TypeVarContext.of_past tvctx,
                TypeIdentifier.of_past id,
                List.map constr_typ_list ~f:(fun (constr, typ) ->
                    match typ with
                    | None -> (Constructor.of_past constr, None)
                    | Some typ ->
                        (Constructor.of_past constr, Some (Typ.of_past typ))) ))
        in
        DatatypeDecl new_id_contr_typ_list_list

  let populate_index =
    let open Or_error.Monad_infix in
    function
    | Definition ((id, typ2), expr) ->
        Typ.populate_index typ2 ~current_type_ast_level:0
          ~current_typevars:String.Map.empty
        >>= fun typ2 ->
        Expr.populate_index expr ~current_ast_level:0
          ~current_identifiers:String.Map.empty ~current_meta_ast_level:0
          ~current_meta_identifiers:String.Map.empty ~current_type_ast_level:0
          ~current_typevars:String.Map.empty
        >>= fun expr -> Ok (Definition ((id, typ2), expr))
    | RecursiveDefinition ((id, typ2), expr) ->
        Typ.populate_index typ2 ~current_type_ast_level:0
          ~current_typevars:String.Map.empty
        >>= fun typ2 ->
        let new_current_identifiers =
          String.Map.set String.Map.empty
            ~key:(ObjIdentifier.get_name id)
            ~data:0
        in
        Expr.populate_index expr ~current_ast_level:1
          ~current_identifiers:new_current_identifiers ~current_meta_ast_level:0
          ~current_meta_identifiers:String.Map.empty ~current_type_ast_level:0
          ~current_typevars:String.Map.empty
        >>= fun expr -> Ok (RecursiveDefinition ((id, typ2), expr))
    | MutualRecursiveDefinition typ_iddef_e_list ->
        let new_current_identifiers =
          List.fold typ_iddef_e_list ~init:String.Map.empty
            ~f:(fun acc ((id, _), _) ->
              String.Map.set acc ~key:(ObjIdentifier.get_name id) ~data:0)
        in
        List.map typ_iddef_e_list ~f:(fun (iddef, e) ->
            IdentifierDefn.populate_index iddef ~current_type_ast_level:0
              ~current_typevars:String.Map.empty
            >>= fun iddef ->
            Expr.populate_index e ~current_ast_level:1
              ~current_identifiers:new_current_identifiers
              ~current_meta_ast_level:0
              ~current_meta_identifiers:String.Map.empty
              ~current_type_ast_level:0 ~current_typevars:String.Map.empty
            >>= fun e -> Ok (iddef, e))
        |> Or_error.combine_errors
        >>= fun typ_iddef_e_list ->
        Ok (MutualRecursiveDefinition typ_iddef_e_list)
    | Expression expr ->
        Expr.populate_index expr ~current_ast_level:0
          ~current_identifiers:String.Map.empty ~current_meta_ast_level:0
          ~current_meta_identifiers:String.Map.empty ~current_type_ast_level:0
          ~current_typevars:String.Map.empty
        >>= fun expr -> Ok (Expression expr)
    | Directive d -> Ok (Directive d)
    | DatatypeDecl id_constr_typ_list_list ->
        (*
            TODO: Modify here for Polymorphic ADT
        *)
        List.map id_constr_typ_list_list
          ~f:(fun (tvctx, typ_id, constr_typ_list) ->
            List.map constr_typ_list ~f:(fun (constr, typ_opt) ->
                match typ_opt with
                | None -> Ok (constr, None)
                | Some typ ->
                    (* If the type doesn't have free variables, then the current ast level is unimportant;
                       if it does, then we raise it by one and bind them here, so no difference. We don't need to
                       cater for the case of an empty tvctx.*)
                    String.Map.of_alist_or_error
                      (List.map ~f:(fun typ -> (TypeVar.get_name typ, 0)) tvctx)
                    >>= fun new_typevars ->
                    Typ.populate_index typ ~current_type_ast_level:1
                      ~current_typevars:new_typevars
                    >>= fun typ -> Ok (constr, Some typ))
            |> Or_error.combine_errors
            >>= fun constr_typ_list -> Ok (tvctx, typ_id, constr_typ_list))
        |> Or_error.combine_errors
        >>= fun id_constr_typ_list_list ->
        Ok (DatatypeDecl id_constr_typ_list_list)
end

and Program : sig
  type t = TopLevelDefn.t list [@@deriving sexp, show, compare, equal]

  val of_past : Past.Program.t -> t
  val populate_index : t -> t Or_error.t
end = struct
  type t = TopLevelDefn.t list [@@deriving sexp, show, compare, equal]

  let of_past progs = List.map progs ~f:TopLevelDefn.of_past

  let populate_index program =
    List.map program ~f:TopLevelDefn.populate_index |> Or_error.combine_errors
end

module TypedTopLevelDefn : sig
  type t =
    | Definition of Typ.t * IdentifierDefn.t * Expr.t
    | RecursiveDefinition of Typ.t * IdentifierDefn.t * Expr.t
    | MutualRecursiveDefinition of (Typ.t * IdentifierDefn.t * Expr.t) list
    | Expression of Typ.t * Expr.t
    | Directive of Directive.t
    | DatatypeDecl of
        (TypeVarContext.t
        * TypeIdentifier.t
        * (Constructor.t * Typ.t option) list)
        list
  [@@deriving sexp, show, compare, equal]

  val convert_from_untyped_without_typecheck : TopLevelDefn.t -> t
end = struct
  (*Note added type for defns (not useful for now but useful for when adding inference) and exprs for the REPL*)
  type t =
    | Definition of Typ.t * IdentifierDefn.t * Expr.t
    | RecursiveDefinition of Typ.t * IdentifierDefn.t * Expr.t
    | MutualRecursiveDefinition of (Typ.t * IdentifierDefn.t * Expr.t) list
    | Expression of Typ.t * Expr.t
    | Directive of Directive.t
    | DatatypeDecl of
        (TypeVarContext.t
        * TypeIdentifier.t
        * (Constructor.t * Typ.t option) list)
        list
  [@@deriving sexp, show, compare, equal]

  let convert_from_untyped_without_typecheck = function
    | TopLevelDefn.Definition (iddef, e) -> Definition (Typ.TUnit, iddef, e)
    | TopLevelDefn.RecursiveDefinition (iddef, e) ->
        RecursiveDefinition (Typ.TUnit, iddef, e)
    | TopLevelDefn.MutualRecursiveDefinition iddef_e_list ->
        MutualRecursiveDefinition
          (List.map ~f:(fun (iddef, e) -> (Typ.TUnit, iddef, e)) iddef_e_list)
    | TopLevelDefn.Expression e -> Expression (Typ.TUnit, e)
    | TopLevelDefn.Directive d -> Directive d
    | TopLevelDefn.DatatypeDecl id_constr_typ_list_list ->
        DatatypeDecl id_constr_typ_list_list
end

module TypedProgram : sig
  type t = TypedTopLevelDefn.t list [@@deriving sexp, show, compare, equal]

  val convert_from_untyped_without_typecheck : Program.t -> t
end = struct
  type t = TypedTopLevelDefn.t list [@@deriving sexp, show, compare, equal]

  let convert_from_untyped_without_typecheck =
    List.map ~f:TypedTopLevelDefn.convert_from_untyped_without_typecheck
end
