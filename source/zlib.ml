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

let _init_fields_out () = (
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

let _make_out: type t.
	(t -> z_fields -> [< z_flush | `PARTIAL_FLUSH | `TREES > `NO_FLUSH] ->
		[`ended | `ok]
	) ->
	t * z_fields * bool ref * (string -> int -> int -> unit) -> string ->
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

let _flush (type t)
	(_, fields, _, output: t * z_fields * bool ref * (string -> int -> int -> unit)
	) =
(
	if fields.next_out_offset > 0 then (
		output (Bytes.unsafe_to_string fields.next_out) 0 fields.next_out_offset;
		reset_next_out fields
	)
);;

let _make_close_out: type t.
	(t -> z_fields -> [< z_flush | `PARTIAL_FLUSH | `TREES > `FINISH] ->
		[`ended | `ok]
	) ->
	(t -> unit) -> (t -> bool) ->
	t * z_fields * bool ref * (string -> int -> int -> unit) -> unit =
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
	fun translate_f close_f closed_f o ->
	let stream, fields, _, _ = o in
	if not (closed_f stream) then (
		fields.next_in <- "";
		fields.next_in_offset <- 0;
		fields.avail_in <- 0;
		loop translate_f close_f o
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

module In_inflater = Zlib__In_inflater;;
module Out_deflater = Zlib__Out_deflater;;
module Out_inflater = Zlib__Out_inflater;;
