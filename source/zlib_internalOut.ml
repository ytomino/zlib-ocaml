open Zlib;;

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
		if rest = 0 then len
		else
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
