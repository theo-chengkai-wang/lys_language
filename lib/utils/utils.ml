open Core

let try_zip_list_or_error l1 l2 or_error =
  let zipped_list_option = List.zip l1 l2 in
  match zipped_list_option with
  | List.Or_unequal_lengths.Ok zipped_list -> Ok zipped_list
  | List.Or_unequal_lengths.Unequal_lengths -> or_error

let rec list_traverse_and_try (xs : 'a list) ~(f : 'a -> 'b option) =
  match xs with
  | [] -> None
  | x :: xs -> (
      match f x with None -> list_traverse_and_try xs ~f | Some y -> Some y)
