open Core

module type Context_S_type = sig
  type key [@@deriving sexp, show, equal, compare]
  type ('a, 'b) s [@@deriving sexp, show, equal, compare]

  val create_empty_context : unit -> (key, 'b) s
  val add_mapping : (key, 'b) s -> key -> 'b -> (key, 'b) s
  val delete_last_mapping : (key, 'b) s -> key -> (key, 'b) s
  val get_last_mapping : (key, 'b) s -> key -> 'b option
end

module type Key_type = sig
  type t [@@deriving sexp, show, equal, compare]
end

module type Context_type = sig
  type ('a, 'b) t [@@deriving sexp, show, equal, compare]

  module type Key_type = Key_type
  module type S = Context_S_type with type ('a, 'b) s = ('a, 'b) t

  module Make : functor (Key : Key_type) -> (S with type key = Key.t)
end

module NaiveContext : Context_type = struct
  type ('a, 'b) t = ('a * 'b) list [@@deriving sexp, show, equal, compare]

  module type Key_type = Key_type
  module type S = Context_S_type with type ('a, 'b) s = ('a, 'b) t

  module Make =
  functor
    (Key : Key_type)
    ->
    struct
      type key = Key.t [@@deriving sexp, show, equal, compare]
      type ('a, 'b) s = ('a, 'b) t [@@deriving sexp, show, equal, compare]

      let create_empty_context () = []
      let add_mapping ctx a b = (a, b) :: ctx

      let rec delete_last_mapping ctx a =
        match ctx with
        | [] -> []
        | (k, v) :: ctx ->
            if Key.equal k a then ctx else (k, v) :: delete_last_mapping ctx a

      let rec get_last_mapping ctx a =
        match ctx with
        | [] -> None
        | (k, v) :: ctx -> if Key.equal k a then Some v else get_last_mapping ctx a
    end
end
