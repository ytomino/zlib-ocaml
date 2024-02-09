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
	| Z_FIXED

type header = [`default | `raw | `gzip]

type out_deflater

val deflate_init_out: ?level: int -> ?strategy: strategy -> ?header: header ->
	(string -> int -> int -> unit) -> out_deflater
val deflate_out: out_deflater -> string -> int -> int -> int
val deflate_output_substring: out_deflater -> string -> int -> int -> unit
val deflate_output_string: out_deflater -> string -> unit
val deflate_flush: out_deflater -> unit
val deflate_end_out: out_deflater -> unit

type in_inflater

val inflate_init_in: ?header: [header | `auto] ->
	(bytes -> int -> int -> int) -> in_inflater
val inflate_in: in_inflater -> bytes -> int -> int -> int
val inflate_end_in: in_inflater -> unit

type out_inflater

val inflate_init_out: ?header: [header | `auto] ->
	(string -> int -> int -> unit) -> out_inflater
val inflate_out: out_inflater -> string -> int -> int -> int
val inflate_flush: out_inflater -> unit
val inflate_end_out: out_inflater -> unit

val crc32_substring: int32 -> string -> int -> int -> int32
val crc32_string: int32 -> string -> int32
