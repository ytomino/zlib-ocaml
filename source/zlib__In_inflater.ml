open Zlib;;

type t = z_stream_inflate * z_fields * (bytes -> int -> int -> int);;

let open_in ?(header: [z_header | `auto] = `auto)
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

let unsafe_input: t -> bytes -> int -> int -> int =
	let rec loop ii len rest = (
		let stream, fields, input = ii in
		if rest = 0
			|| (
				fields.avail_in = 0
				&& (
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

let input (ii: t) (s: bytes) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && len <= Bytes.length s - pos
	then unsafe_input ii s pos len
	else invalid_arg "Zlib.In_inflater.input" (* __FUNCTION__ *)
);;

let close_in (ii: t) = (
	let stream, fields, _ = ii in
	fields.next_out <- Bytes.empty;
	fields.avail_out <- 0;
	inflate_close stream
);;
