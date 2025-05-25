open Zlib

type t

val open_in: ?header:[z_header | `auto] -> (bytes -> int -> int -> int) -> t
val input: t -> bytes -> int -> int -> int
val close_in: t -> unit
