(** For internal use *)

open Zlib

val _init_fields_out: unit -> z_fields
val _make_out:
	('a -> z_fields -> [< z_flush | `PARTIAL_FLUSH | `TREES > `NO_FLUSH] ->
		[`ended | `ok ]
	) ->
	'a * z_fields * bool ref * (string -> int -> int -> unit) -> string ->
	int -> int -> int
val _flush: 'a * z_fields * bool ref * (string -> int -> int -> unit) -> unit
val _make_close_out:
	('a -> z_fields -> [< z_flush | `PARTIAL_FLUSH | `TREES > `FINISH] ->
		[`ended | `ok ]
	) ->
	('a -> unit) -> ('a -> bool) ->
	'a * z_fields * bool ref * (string -> int -> int -> unit) -> unit
