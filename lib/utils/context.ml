open Core

module type Context_type = sig
  type ('a, 'b) s [@@deriving sexp, equal, compare]

  module type Key_type = sig
    type t [@@deriving sexp, equal, compare]
  end

  module type S = sig
    module Key : Key_type

    type 'b t = (Key.t, 'b) s [@@deriving sexp, equal, compare]

    val create_empty_context : unit -> 'b t
    val add_mapping : 'b t -> Key.t -> 'b -> 'b t

    (* val delete_last_mapping : 'b t -> Key.t -> 'b t *)
    val get_last_mapping : 'b t -> Key.t -> 'b option
    val add_all_mappings : 'b t -> (Key.t * 'b) list -> 'b t
    val is_in_context : 'b t -> Key.t -> bool
    val get_all_mappings_as_list: 'b t -> (Key.t * 'b) list
  end

  module Make : functor (Key : Key_type) -> S with module Key = Key
end

module NaiveContext : Context_type = struct
  type ('a, 'b) s = ('a * 'b) list [@@deriving sexp, equal, compare]

  module type Key_type = sig
    type t [@@deriving sexp, equal, compare]
  end

  module type S = sig
    module Key : Key_type

    type 'b t = (Key.t, 'b) s [@@deriving sexp, equal, compare]

    val create_empty_context : unit -> 'b t
    val add_mapping : 'b t -> Key.t -> 'b -> 'b t

    (* val delete_last_mapping : 'b t -> Key.t -> 'b t *)
    val get_last_mapping : 'b t -> Key.t -> 'b option
    val add_all_mappings : 'b t -> (Key.t * 'b) list -> 'b t
    val is_in_context : 'b t -> Key.t -> bool
    val get_all_mappings_as_list: 'b t -> (Key.t * 'b) list

  end

  module Make =
  functor
    (Key : Key_type)
    ->
    struct
      module Key = Key

      type 'b t = (Key.t, 'b) s [@@deriving sexp, equal, compare]

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
        | (k, v) :: ctx ->
            if Key.equal k a then Some v else get_last_mapping ctx a

      let add_all_mappings ctx xs = xs @ ctx
      let is_in_context ctx k = List.exists ctx ~f:(fun (x, _) -> Key.equal k x)
      let get_all_mappings_as_list ctx = ctx
    end
end
