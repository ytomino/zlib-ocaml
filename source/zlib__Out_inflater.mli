open Zlib

type t

val open_out: ?header:[z_header | `auto] -> (string -> int -> int -> unit) -> t
val out_substring: t -> string -> int -> int -> int
val flush: t -> unit
val close_out: t -> unit

val is_inflated_out: t -> bool
(** [true] means the inflating is finished internally.
    It does not require any more calling of [out_substring], but the data may
    be remained, so [close_out] or [flush] should be called. *)
