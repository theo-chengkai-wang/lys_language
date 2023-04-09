open Core

(* Hack for show-able map *)
include Map.S with type Key.t = String.t [@@deriving show]
