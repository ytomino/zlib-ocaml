external zlib_get_version_string: unit -> string =
	"mlzlib_get_version_string";;

type flush =
	| Z_NO_FLUSH (* 0 *)
	| Z_PARTIAL_FLUSH (* 1 *)
	| Z_SYNC_FLUSH (* 2 *)
	| Z_FULL_FLUSH (* 3 *)
	| Z_FINISH (* 4 *)
	| Z_BLOCK (* 5 *)
	[@@ocaml.warning "-37"];; (* suppress "Unused constructor." *)

type level = int;;

let z_no_compression = 0;;
let z_best_speed = 1;;
let z_best_compression = 9;;
let z_default_compression = -1;;

type strategy =
	| Z_DEFAULT_STRATEGY (* 0 *)
	| Z_FILTERED (* 1 *)
	| Z_HUFFMAN_ONLY (* 2 *)
	| Z_RLE (* 3 *)
	| Z_FIXED;; (* 4 *)

type z_stream_s;;

external avail_in: z_stream_s -> int = "mlzlib_avail_in";;
external set_in: z_stream_s -> string -> int -> int -> unit = "mlzlib_set_in";;
external avail_out: z_stream_s -> int = "mlzlib_avail_out";;
external set_out: z_stream_s -> bytes -> int -> int -> unit =
	"mlzlib_set_out";;

external deflate_init: int -> strategy -> int -> z_stream_s =
	"mlzlib_deflate_init";;
external deflate: z_stream_s -> flush -> bool = "mlzlib_deflate";;
external deflate_end: z_stream_s -> unit = "mlzlib_deflate_end";;

external inflate_init: int -> z_stream_s = "mlzlib_inflate_init";;
external inflate: z_stream_s -> flush -> bool = "mlzlib_inflate";;
external inflate_end: z_stream_s -> unit = "mlzlib_inflate_end";;

type header = [`default | `raw | `gzip];;

type writer = z_stream_s * bytes * (string -> int -> int -> unit);;

let deflate_init
	?(level: int = z_default_compression)
	?(strategy: strategy = Z_DEFAULT_STRATEGY)
	?(header: header = `default)
	(output: string -> int -> int -> unit): writer =
(
	let window_bits = (
		match header with
		| `default -> 15
		| `raw -> -15
		| `gzip -> 31
	) in
	let stream = deflate_init level strategy window_bits in
	let buffer = Bytes.create (1 lsl 15) in
	set_out stream buffer 0 (Bytes.length buffer);
	stream, buffer, output
);;

let deflate (writer: writer) (s: string) (pos: int) (len: int): unit = (
	let stream, buffer, output = writer in
	assert (avail_in stream = 0);
	set_in stream s pos len;
	while avail_in stream > 0 do
		let (_: bool) = deflate stream Z_NO_FLUSH in
		if avail_out stream = 0 then (
			output (Bytes.unsafe_to_string buffer) 0 (Bytes.length buffer);
			set_out stream buffer 0 (Bytes.length buffer)
		)
	done
);;

let deflate_end (writer: writer): unit = (
	let stream, buffer, output = writer in
	assert (avail_in stream = 0);
	while not (deflate stream Z_FINISH) do
		assert (avail_out stream = 0);
		output (Bytes.unsafe_to_string buffer) 0 (Bytes.length buffer);
		set_out stream buffer 0 (Bytes.length buffer)
	done;
	let rest = avail_out stream in
	let used = Bytes.length buffer - rest in
	output (Bytes.unsafe_to_string buffer) 0 used;
	deflate_end stream
);;

type reader = z_stream_s * bytes * (bytes -> int -> int -> int);;

let inflate_init ?(header: [header | `auto] = `auto)
	(input: bytes -> int -> int -> int): reader =
(
	let window_bits = (
		match header with
		| `default -> 15
		| `raw -> -15
		| `gzip -> 31
		| `auto -> 47
	) in
	let stream = inflate_init window_bits in
	let buffer = Bytes.create (1 lsl 15) in
	stream, buffer, input
);;

let inflate (reader: reader) (s: bytes) (pos: int) (len: int): int = (
	let stream, buffer, input = reader in
	set_out stream s pos len;
	while
		avail_out stream > 0 && (
			if avail_in stream = 0 then (
				set_in stream (Bytes.unsafe_to_string buffer) 0
					(input buffer 0 (Bytes.length buffer));
			);
			avail_in stream > 0)
	do
		let (_: bool) = inflate stream Z_NO_FLUSH in
		()
	done;
	let rest = avail_out stream in
	let used = len - rest in
	used
);;

let inflate_end (reader: reader): unit = (
	let stream, _, _ = reader in
	inflate_end stream
);;

external crc32_substring: int32 -> string -> int -> int -> int32 =
	"mlzlib_crc32_substring"
