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

type strategy = [
	| `DEFAULT_STRATEGY
	| `FILTERED
	| `HUFFMAN_ONLY
	| `RLE
	| `FIXED
];;

type z_stream_s;;

type fields = {
	mutable next_in: string;
	mutable next_in_offset: int;
	mutable avail_in: int;
	mutable next_out: bytes;
	mutable next_out_offset: int;
	mutable avail_out: int;
	mutable stream_end: bool
} [@@ocaml.warning "-69"] (* suppress "Unused record field." *);;

external ended: z_stream_s -> bool = "mlzlib_ended";;

external deflate_init: int -> strategy -> int -> z_stream_s =
	"mlzlib_deflate_init";;
external deflate: z_stream_s -> fields -> flush -> bool = "mlzlib_deflate";;
external deflate_end: z_stream_s -> fields -> unit = "mlzlib_deflate_end";;

external inflate_init: int -> z_stream_s = "mlzlib_inflate_init";;
external inflate: z_stream_s -> fields -> flush -> bool = "mlzlib_inflate";;
external inflate_end: z_stream_s -> fields -> unit = "mlzlib_inflate_end";;

type header = [`default | `raw | `gzip];;

let window_bits_of_header (header: [< header | `auto]) = (
	match header with
	| `default -> 15
	| `raw -> -15
	| `gzip -> 31
	| `auto -> 47
);;

let init_fields_out () = (
	let next_out = Bytes.create (1 lsl 15) in
	{
		next_in = "";
		next_in_offset = 0;
		avail_in = 0;
		next_out;
		next_out_offset = 0;
		avail_out = Bytes.length next_out;
		stream_end = false
	}
);;

let reset_next_out (fields: fields) = (
	fields.next_out_offset <- 0;
	let next_out_length = Bytes.length fields.next_out in
	fields.next_out <- Bytes.create next_out_length;
	fields.avail_out <- next_out_length
);;

let make_out (translate_f: z_stream_s -> fields -> flush -> bool)
	(stream, fields, output: z_stream_s * fields * (string -> int -> int -> unit))
	(s: string) (pos: int) (len: int) =
(
	fields.next_in <- s;
	fields.next_in_offset <- pos;
	fields.avail_in <- len;
	let rec loop rest = (
		if rest = 0 then rest else
		let stream_end = translate_f stream fields Z_NO_FLUSH in
		if fields.avail_out = 0 then (
			output (Bytes.unsafe_to_string fields.next_out) 0 fields.next_out_offset;
			reset_next_out fields
		);
		let rest = fields.avail_in in
		if stream_end then rest
		else loop rest
	) in
	let rest = loop len in
	let used = len - rest in
	assert (used = fields.next_in_offset - pos);
	used
);;

let make_end_out (translate_f: z_stream_s -> fields -> flush -> bool)
	(end_f: z_stream_s -> fields -> unit)
	(stream, fields, output: z_stream_s * fields * (string -> int -> int -> unit))
	=
(
	if not (ended stream) then (
		fields.next_in <- "";
		fields.avail_in <- 0;
		let rec loop () = (
			match translate_f stream fields Z_FINISH  with
			| _ as stream_end ->
				if fields.next_out_offset > 0 then (
					output (Bytes.unsafe_to_string fields.next_out) 0 fields.next_out_offset
				);
				if stream_end then None
				else (
					if fields.next_out_offset > 0 then (
						reset_next_out fields
					);
					loop ()
				)
			| exception (Failure _ as exn) -> Some exn
		) in
		let exn_opt = loop () in
		end_f stream fields;
		match exn_opt with
		| Some exn -> raise exn
		| None -> ()
	)
);;

type out_deflater = z_stream_s * fields * (string -> int -> int -> unit);;

let deflate_init_out ?(level: int = z_default_compression)
	?(strategy: strategy = `DEFAULT_STRATEGY) ?(header: header = `default)
	(output: string -> int -> int -> unit) =
(
	let window_bits = window_bits_of_header header in
	let stream = deflate_init level strategy window_bits in
	stream, init_fields_out (), output
);;

let unsafe_deflate_out = make_out deflate;;

let deflate_out (od: out_deflater) (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && pos + len <= String.length s
	then unsafe_deflate_out od s pos len
	else invalid_arg "Zlib.deflate_out" (* __FUNCTION__ *)
);;

let deflate_output_substring (od: out_deflater) (s: string) (pos: int)
	(len: int) =
(
	let loc = "Zlib.deflate_output_substring" (* __FUNCTION__ *) in
	if pos >= 0 && len >= 0 && pos + len <= String.length s then (
		let r = unsafe_deflate_out od s pos len in
		if r <> len then failwith loc
	) else invalid_arg loc
);;

let deflate_output_string (od: out_deflater) (s: string) = (
	let len = String.length s in
	let r = unsafe_deflate_out od s 0 len in
	if r <> len then failwith "Zlib.deflate_output_string" (* __FUNCTION__ *)
);;

let deflate_flush
	(_, fields, output: z_stream_s * fields * (string -> int -> int -> unit)) =
(
	if fields.next_out_offset > 0 then (
		output (Bytes.unsafe_to_string fields.next_out) 0 fields.next_out_offset;
		reset_next_out fields
	)
);;

let deflate_end_out = make_end_out deflate deflate_end;;

type in_inflater = z_stream_s * fields * (bytes -> int -> int -> int);;

let inflate_init_in ?(header: [header | `auto] = `auto)
	(input: bytes -> int -> int -> int) =
(
	let window_bits = window_bits_of_header header in
	let stream = inflate_init window_bits in
	let next_in = Bytes.unsafe_to_string (Bytes.create (1 lsl 15)) in
	let fields = {
		next_in;
		next_in_offset = 0;
		avail_in = 0;
		next_out = Bytes.empty;
		next_out_offset = 0;
		avail_out = 0;
		stream_end = false
	}
	in
	stream, fields, input
);;

let unsafe_inflate_in
	(stream, fields, input: z_stream_s * fields * (bytes -> int -> int -> int))
	(s: bytes) (pos: int) (len: int) =
(
	fields.next_out <- s;
	fields.next_out_offset <- pos;
	fields.avail_out <- len;
	let rec loop rest = (
		if rest = 0
			|| (
				fields.avail_in = 0 && (
					let rest_in =
						input (Bytes.unsafe_of_string fields.next_in) 0 (String.length fields.next_in)
					in
					fields.next_in_offset <- 0;
					fields.avail_in <- rest_in;
					rest_in = 0
				)
			)
		then rest
		else
		let stream_end = inflate stream fields Z_NO_FLUSH in
		let rest = fields.avail_out in
		if stream_end then rest
		else loop rest
	) in
	let rest = loop len in
	let used = len - rest in
	assert (used = fields.next_out_offset - pos);
	used
);;

let inflate_in (ii: in_inflater) (s: bytes) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && pos + len <= Bytes.length s
	then unsafe_inflate_in ii s pos len
	else invalid_arg "Zlib.inflate_in" (* __FUNCTION__ *)
);;

let inflate_end_in (stream, fields, _: in_inflater) = (
	fields.next_out <- Bytes.empty;
	fields.avail_out <- 0;
	inflate_end stream fields
);;

type out_inflater = z_stream_s * fields * (string -> int -> int -> unit);;

let inflate_init_out ?(header: [header | `auto] = `auto)
	(output: string -> int -> int -> unit) =
(
	let window_bits = window_bits_of_header header in
	let stream = inflate_init window_bits in
	stream, init_fields_out (), output
);;

let unsafe_inflate_out = make_out inflate;;

let inflate_out (oi: out_inflater) (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && pos + len <= String.length s
	then unsafe_inflate_out oi s pos len
	else invalid_arg "Zlib.inflate_out" (* __FUNCTION__ *)
);;

let inflate_flush = deflate_flush;;

let inflate_end_out = make_end_out inflate inflate_end;;

let is_inflated_out
	(_, fields, _: z_stream_s * fields * (string -> int -> int -> unit)) =
(
	fields.stream_end
);;

external unsafe_crc32_substring: int32 -> string -> int -> int -> int32 =
	"mlzlib_unsafe_crc32_substring"

let crc32_substring (crc: int32) (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && pos + len <= String.length s
	then unsafe_crc32_substring crc s pos len
	else invalid_arg "Zlib.crc32_substring" (* __FUNCTION__ *)
);;

let crc32_string (crc: int32) (s: string) = (
	unsafe_crc32_substring crc s 0 (String.length s)
);;
