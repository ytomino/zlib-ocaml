external zlib_get_version_string: unit -> string = "mlzlib_get_version_string"

type level = int

val z_no_compression: int
val z_best_speed: int
val z_best_compression: int
val z_default_compression: int

type strategy =
	| Z_DEFAULT_STRATEGY
	| Z_FILTERED
	| Z_HUFFMAN_ONLY
	| Z_RLE
	| Z_FIXED;;

type header = [`default | `raw | `gzip]

type writer

val deflate_init: ?level: int -> ?strategy: strategy ->
	?header: header ->
	(string -> int -> int -> unit) -> writer
val deflate: writer -> string -> int -> int -> unit
val deflate_end: writer -> unit

type reader

val inflate_init: ?header: [header | `auto] ->
	(bytes -> int -> int -> int) -> reader
val inflate: reader -> bytes -> int -> int -> int
val inflate_end: reader -> unit
