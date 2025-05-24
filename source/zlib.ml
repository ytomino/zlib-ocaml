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

type z_header = [`default | `raw | `gzip];;

external closed: z_stream_s -> bool = "mlzlib_closed";;

external deflate_init: level:int -> strategy:z_strategy -> header:z_header ->
	unit -> z_stream_s =
	"mlzlib_deflate_init";;
external deflate: z_stream_s -> z_fields -> [z_flush | `PARTIAL_FLUSH] ->
	[> `ended | `ok] =
	"mlzlib_deflate";;
external deflate_close: z_stream_s -> unit = "mlzlib_deflate_close";;

external inflate_init: header:[z_header | `auto] -> unit -> z_stream_s =
	"mlzlib_inflate_init";;
external inflate: z_stream_s -> z_fields -> z_flush -> [> `ended | `ok] =
	"mlzlib_inflate";;
external inflate_close: z_stream_s -> unit = "mlzlib_inflate_close";;

let init_fields_out () = (
	let next_out = Bytes.create (1 lsl 15) in
	{
		next_in = "";
		next_in_offset = 0;
		avail_in = 0;
		next_out;
		next_out_offset = 0;
		avail_out = Bytes.length next_out
	}
);;

let reset_next_out (fields: z_fields) = (
	fields.next_out_offset <- 0;
	let next_out_length = Bytes.length fields.next_out in
	fields.next_out <- Bytes.create next_out_length;
	fields.avail_out <- next_out_length
);;

let make_out:
	(z_stream_s -> z_fields -> [< z_flush | `PARTIAL_FLUSH > `NO_FLUSH] ->
		[`ended | `ok]
	) ->
	z_stream_s * z_fields * bool ref * (string -> int -> int -> unit) -> string ->
	int -> int -> int =
	let rec loop translate_f o len rest = (
		if rest = 0 then len else
		let stream, fields, stream_end_ref, output = o in
		let translated = translate_f stream fields `NO_FLUSH in
		if fields.avail_out = 0 then (
			output (Bytes.unsafe_to_string fields.next_out) 0 fields.next_out_offset;
			reset_next_out fields
		);
		let rest = fields.avail_in in
		match translated with
		| `ended ->
			stream_end_ref := true;
			len - rest
		| `ok ->
			loop translate_f o len rest
	) in
	fun translate_f o s pos len ->
	let _, fields, _, _ = o in
	fields.next_in <- s;
	fields.next_in_offset <- pos;
	fields.avail_in <- len;
	let used = loop translate_f o len len in
	assert (used = fields.next_in_offset - pos);
	used;;

let make_end_out:
	(z_stream_s -> z_fields -> [< z_flush | `PARTIAL_FLUSH > `FINISH] ->
		[`ended | `ok]
	) ->
	(z_stream_s -> unit) ->
	z_stream_s * z_fields * bool ref * (string -> int -> int -> unit) -> unit =
	let rec loop translate_f close_f o = (
		let stream, fields, stream_end_ref, output = o in
		match translate_f stream fields `FINISH  with
		| _ as translated ->
			if fields.next_out_offset > 0 then (
				output (Bytes.unsafe_to_string fields.next_out) 0 fields.next_out_offset
			);
			begin match translated with
			| `ended ->
				stream_end_ref := true;
				close_f stream
			| `ok ->
				if fields.next_out_offset > 0 then (
					reset_next_out fields
				);
				loop translate_f close_f o
			end
		| exception (Failure _ as exn) ->
			close_f stream;
			raise exn
	) in
	fun translate_f close_f o ->
	let stream, fields, _, _ = o in
	if not (closed stream) then (
		fields.next_in <- "";
		fields.avail_in <- 0;
		loop translate_f close_f o
	);;

type out_deflater =
	z_stream_s * z_fields * bool ref * (string -> int -> int -> unit);;

let deflate_init_out ?(level: int = z_default_compression)
	?(strategy: z_strategy = `DEFAULT_STRATEGY) ?(header: z_header = `default)
	(output: string -> int -> int -> unit) =
(
	let stream = deflate_init ~level ~strategy ~header () in
	stream, init_fields_out (), ref false, output
);;

let unsafe_deflate_out = make_out deflate;;

let deflate_out (od: out_deflater) (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && len <= String.length s - pos
	then unsafe_deflate_out od s pos len
	else invalid_arg "Zlib.deflate_out" (* __FUNCTION__ *)
);;

let deflate_output_substring (od: out_deflater) (s: string) (pos: int)
	(len: int) =
(
	let loc = "Zlib.deflate_output_substring" (* __FUNCTION__ *) in
	if pos >= 0 && len >= 0 && len <= String.length s - pos then (
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
	(_, fields, _, output:
		z_stream_s * z_fields * bool ref * (string -> int -> int -> unit)
	) =
(
	if fields.next_out_offset > 0 then (
		output (Bytes.unsafe_to_string fields.next_out) 0 fields.next_out_offset;
		reset_next_out fields
	)
);;

let deflate_end_out = make_end_out deflate deflate_close;;

type in_inflater = z_stream_s * z_fields * (bytes -> int -> int -> int);;

let inflate_init_in ?(header: [z_header | `auto] = `auto)
	(input: bytes -> int -> int -> int) =
(
	let stream = inflate_init ~header () in
	let next_in = Bytes.unsafe_to_string (Bytes.create (1 lsl 15)) in
	let fields = {
		next_in;
		next_in_offset = 0;
		avail_in = 0;
		next_out = Bytes.empty;
		next_out_offset = 0;
		avail_out = 0
	}
	in
	stream, fields, input
);;

let unsafe_inflate_in: in_inflater -> bytes -> int -> int -> int =
	let rec loop ii len rest = (
		let stream, fields, input = ii in
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
		then len - rest
		else
		let inflated = inflate stream fields `NO_FLUSH in
		let rest = fields.avail_out in
		match inflated with
		| `ended ->
			len - rest
		| `ok ->
			loop ii len rest
	) in
	fun ii s pos len ->
	let _, fields, _ = ii in
	fields.next_out <- s;
	fields.next_out_offset <- pos;
	fields.avail_out <- len;
	let used = loop ii len len in
	assert (used = fields.next_out_offset - pos);
	used;;

let inflate_in (ii: in_inflater) (s: bytes) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && len <= Bytes.length s - pos
	then unsafe_inflate_in ii s pos len
	else invalid_arg "Zlib.inflate_in" (* __FUNCTION__ *)
);;

let inflate_end_in (stream, fields, _: in_inflater) = (
	fields.next_out <- Bytes.empty;
	fields.avail_out <- 0;
	inflate_close stream
);;

type out_inflater =
	z_stream_s * z_fields * bool ref * (string -> int -> int -> unit);;

let inflate_init_out ?(header: [z_header | `auto] = `auto)
	(output: string -> int -> int -> unit) =
(
	let stream = inflate_init ~header () in
	stream, init_fields_out (), ref false, output
);;

let unsafe_inflate_out = make_out inflate;;

let inflate_out (oi: out_inflater) (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && len <= String.length s - pos
	then unsafe_inflate_out oi s pos len
	else invalid_arg "Zlib.inflate_out" (* __FUNCTION__ *)
);;

let inflate_flush = deflate_flush;;

let inflate_end_out = make_end_out inflate inflate_close;;

let is_inflated_out
	(_, _, stream_end_ref, _:
		z_stream_s * z_fields * bool ref * (string -> int -> int -> unit)
	) =
(
	!stream_end_ref
);;

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
