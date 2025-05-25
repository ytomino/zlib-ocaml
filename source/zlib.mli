external zlib_get_version_string: unit -> string = "mlzlib_get_version_string"

type z_flush = [
	| `NO_FLUSH
	| `SYNC_FLUSH
	| `FULL_FLUSH
	| `FINISH
	| `BLOCK
]

val z_no_compression: int
val z_best_speed: int
val z_best_compression: int
val z_default_compression: int

type z_strategy = [
	| `DEFAULT_STRATEGY
	| `FILTERED
	| `HUFFMAN_ONLY
	| `RLE
	| `FIXED
]

type z_fields = {
	mutable next_in: string;
	mutable next_in_offset: int;
	mutable avail_in: int;
	mutable next_out: bytes;
	mutable next_out_offset: int;
	mutable avail_out: int
}

type z_header = [`default | `raw | `gzip]

type z_stream_deflate

external deflate_init: level:int -> strategy:z_strategy -> header:z_header ->
	unit -> z_stream_deflate =
	"mlzlib_deflate_init"
val deflate: z_stream_deflate -> z_fields -> [z_flush | `PARTIAL_FLUSH] ->
	[> `ended | `ok]
external deflate_close: z_stream_deflate -> unit = "mlzlib_deflate_close"

type z_stream_inflate

external inflate_init: header:[z_header | `auto] -> unit -> z_stream_inflate =
	"mlzlib_inflate_init"
val inflate: z_stream_inflate -> z_fields -> [z_flush | `TREES] ->
	[> `ended | `ok]
external inflate_close: z_stream_inflate -> unit = "mlzlib_inflate_close"

val crc32_substring: int32 -> string -> int -> int -> int32
val crc32_string: int32 -> string -> int32

module In_inflater = Zlib__In_inflater
module Out_deflater = Zlib__Out_deflater
module Out_inflater = Zlib__Out_inflater
