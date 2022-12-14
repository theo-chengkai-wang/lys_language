module type Context_S_type = sig
  type key [@@deriving sexp, show, equal, compare]
  type ('a, 'b) t [@@deriving sexp, show, equal, compare]

  val create_empty_context : unit -> (key, 'b) t
  val add_mapping : (key, 'b) t -> key -> 'b -> (key, 'b) t
  val delete_last_mapping : (key, 'b) t -> key -> (key, 'b) t
  val get_last_mapping : (key, 'b) t -> key -> 'b option
  val add_all_mappings: (key, 'b) t -> (key * 'b) list -> (key, 'b) t
end

module type Key_type = sig
  type t [@@deriving sexp, show, equal, compare]
end

module type Context_type = sig
  type ('a, 'b) s [@@deriving sexp, show, equal, compare]

  module type Key_type = Key_type
  module type S = Context_S_type with type ('a, 'b) t = ('a, 'b) s

  module Make : functor (Key : Key_type) -> (S with type key = Key.t)
end

module NaiveContext : Context_type
