external zlib_get_version_string: unit -> string =
	"mlzlib_get_version_string";;

type z_flush = [
	| `NO_FLUSH
	| `SYNC_FLUSH
	| `FULL_FLUSH
	| `FINISH
	| `BLOCK
];;

let z_no_compression = 0;;
let z_best_speed = 1;;
let z_best_compression = 9;;
let z_default_compression = -1;;

type z_strategy = [
	| `DEFAULT_STRATEGY
	| `FILTERED
	| `HUFFMAN_ONLY
	| `RLE
	| `FIXED
];;

type z_stream_s;;

type z_fields = {
	mutable next_in: string;
	mutable next_in_offset: int;
	mutable avail_in: int;
	mutable next_out: bytes;
	mutable next_out_offset: int;
	mutable avail_out: int
};;

let valid_in (fields: z_fields) = (
	let {next_in; next_in_offset; avail_in; _} = fields in
	next_in_offset >= 0 && avail_in >= 0
	&& avail_in <= String.length next_in - next_in_offset
);;

let valid_out (fields: z_fields) = (
	let {next_out; next_out_offset; avail_out; _} = fields in
	next_out_offset >= 0 && avail_out >= 0
	&& avail_out <= Bytes.length next_out - next_out_offset
);;

type z_header = [`default | `raw | `gzip];;

type z_stream_deflate = z_stream_s;;

external deflate_init: level:int -> strategy:z_strategy -> header:z_header ->
	unit -> z_stream_deflate =
	"mlzlib_deflate_init";;

external unsafe_deflate: z_stream_deflate -> z_fields ->
	[z_flush | `PARTIAL_FLUSH] -> [> `ended | `ok] =
	"mlzlib_deflate";;

let deflate (stream: z_stream_deflate) (fields: z_fields)
	(flush: [z_flush | `PARTIAL_FLUSH]) =
(
	if valid_in fields && valid_out fields
	then unsafe_deflate stream fields flush
	else invalid_arg "Zlib.deflate" (* __FUNCTION__ *)
);;

external deflate_close: z_stream_deflate -> unit = "mlzlib_deflate_close";;

type z_stream_inflate = z_stream_s;;

external inflate_init: header:[z_header | `auto] -> unit -> z_stream_inflate =
	"mlzlib_inflate_init";;

external unsafe_inflate: z_stream_inflate -> z_fields -> [z_flush | `TREES] ->
	[> `ended | `ok] =
	"mlzlib_inflate";;

let inflate (stream: z_stream_inflate) (fields: z_fields)
	(flush: [z_flush | `TREES]) =
(
	if valid_in fields && valid_out fields
	then unsafe_inflate stream fields flush
	else invalid_arg "Zlib.inflate" (* __FUNCTION__ *)
);;

external inflate_close: z_stream_inflate -> unit = "mlzlib_inflate_close";;

external unsafe_crc32_substring: int32 -> string -> int -> int -> int32 =
	"mlzlib_unsafe_crc32_substring"

let crc32_substring (crc: int32) (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && len <= String.length s - pos
	then unsafe_crc32_substring crc s pos len
	else invalid_arg "Zlib.crc32_substring" (* __FUNCTION__ *)
);;

let crc32_string (crc: int32) (s: string) = (
	unsafe_crc32_substring crc s 0 (String.length s)
);;

module In_inflater = Zlib__In_inflater;;
module Out_deflater = Zlib__Out_deflater;;
module Out_inflater = Zlib__Out_inflater;;
