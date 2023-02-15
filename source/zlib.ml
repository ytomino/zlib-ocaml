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

let window_bits_of_header (header: [< header | `auto]) = (
	match header with
	| `default -> 15
	| `raw -> -15
	| `gzip -> 31
	| `auto -> 47
);;

let make_out (translate_f: z_stream_s -> flush -> bool)
	(stream, buffer, output: z_stream_s * bytes * (string -> int -> int -> unit))
	(s: string) (pos: int) (len: int) =
(
	set_in stream s pos len;
	let rec loop rest = (
		if rest = 0 then rest else
		let stream_end = translate_f stream Z_NO_FLUSH in
		if avail_out stream = 0 then (
			output (Bytes.unsafe_to_string buffer) 0 (Bytes.length buffer);
			set_out stream buffer 0 (Bytes.length buffer)
		);
		let rest = avail_in stream in
		if stream_end then rest
		else loop rest
	) in
	let rest = loop len in
	let used = len - rest in
	used
);;

let make_end_out (translate_f: z_stream_s -> flush -> bool)
	(end_f: z_stream_s -> unit)
	(stream, buffer, output: z_stream_s * bytes * (string -> int -> int -> unit)) =
(
	set_in stream "" 0 0;
	let rec loop () = (
		let stream_end = translate_f stream Z_FINISH in
		let used_out = Bytes.length buffer - avail_out stream in
		if used_out > 0 then (
			output (Bytes.unsafe_to_string buffer) 0 used_out
		);
		if stream_end then ()
		else (
			if used_out > 0 then set_out stream buffer 0 (Bytes.length buffer);
			loop ()
		)
	) in
	loop ();
	end_f stream
);;

type out_deflater = z_stream_s * bytes * (string -> int -> int -> unit);;

let deflate_init_out ?(level: int = z_default_compression)
	?(strategy: strategy = Z_DEFAULT_STRATEGY) ?(header: header = `default)
	(output: string -> int -> int -> unit) =
(
	let window_bits = window_bits_of_header header in
	let stream = deflate_init level strategy window_bits in
	let buffer = Bytes.create (1 lsl 15) in
	set_out stream buffer 0 (Bytes.length buffer);
	stream, buffer, output
);;

let deflate_out = make_out deflate;;

let deflate_end_out = make_end_out deflate deflate_end;;

type in_inflater = z_stream_s * bytes * (bytes -> int -> int -> int);;

let inflate_init_in ?(header: [header | `auto] = `auto)
	(input: bytes -> int -> int -> int) =
(
	let window_bits = window_bits_of_header header in
	let stream = inflate_init window_bits in
	let buffer = Bytes.create (1 lsl 15) in
	stream, buffer, input
);;

let inflate_in (reader: in_inflater) (s: bytes) (pos: int) (len: int) = (
	let stream, buffer, input = reader in
	set_out stream s pos len;
	let rec loop rest = (
		if rest = 0
			|| (
				avail_in stream = 0 && (
					let rest_in = input buffer 0 (Bytes.length buffer) in
					set_in stream (Bytes.unsafe_to_string buffer) 0 rest_in;
					rest_in = 0
				)
			)
		then rest
		else
		let stream_end = inflate stream Z_NO_FLUSH in
		let rest = avail_out stream in
		if stream_end then rest
		else loop rest
	) in
	let rest = loop len in
	let used = len - rest in
	used
);;

let inflate_end_in (reader: in_inflater) = (
	let stream, _, _ = reader in
	inflate_end stream
);;

type out_inflater = z_stream_s * bytes * (string -> int -> int -> unit);;

let inflate_init_out ?(header: [header | `auto] = `auto)
	(output: string -> int -> int -> unit) =
(
	let window_bits = window_bits_of_header header in
	let stream = inflate_init window_bits in
	let buffer = Bytes.create (1 lsl 15) in
	set_out stream buffer 0 (Bytes.length buffer);
	stream, buffer, output
);;

let inflate_out = make_out inflate;;

let inflate_end_out = make_end_out inflate inflate_end;;

external crc32_substring: int32 -> string -> int -> int -> int32 =
	"mlzlib_crc32_substring"
