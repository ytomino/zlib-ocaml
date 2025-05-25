open Zlib

type t

val open_out: ?level:int -> ?strategy:z_strategy -> ?header:z_header ->
	(string -> int -> int -> unit) -> t
val out_substring: t -> string -> int -> int -> int
val output_substring: t -> string -> int -> int -> unit
val output_string: t -> string -> unit
val flush: t -> unit
val close_out: t -> unit
