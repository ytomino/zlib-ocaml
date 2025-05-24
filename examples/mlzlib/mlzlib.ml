let max_wbits = 15;;

type z_level = Z_DEFAULT_COMPRESSION;;

let zlib_deflateInit level = (
	let level =
		match level with
		| Z_DEFAULT_COMPRESSION -> Zlib.z_default_compression
	in
	Zlib.deflate_init ~level ~strategy:`DEFAULT_STRATEGY ~header:`gzip ()
);;

let zlib_inflateInit2 window_bits = (
	let header =
		match window_bits with
		| 15 -> `default
		| -15 -> `raw
		| 31 -> `gzip
		| 47 -> `auto
		| _ -> assert false
	in
	Zlib.inflate_init ~header ()
);;

let zlib_inflateInit () = Zlib.inflate_init ~header:`auto ();;

let zlib_deflateEnd = Zlib.deflate_close;;

let zlib_inflateEnd = Zlib.inflate_close;;

type z_flush = Z_NO_FLUSH | Z_SYNC_FLUSH | Z_FINISH;;

let make_deflate_or_inflate translate_f strm flush in_s in_offset in_length
	out_s out_offset out_length =
(
	let fields = {
		Zlib.next_in = in_s;
		next_in_offset = in_offset;
		avail_in = in_length;
		next_out = out_s;
		next_out_offset = out_offset;
		avail_out = out_length
	}
	in
	let flush =
		match flush with
		| Z_NO_FLUSH -> `NO_FLUSH
		| Z_SYNC_FLUSH -> `SYNC_FLUSH
		| Z_FINISH -> `FINISH
	in
	let stream_end =
		match translate_f strm fields flush with
		| `ended -> true
		| `ok -> false
	in
	let used_in = in_length - fields.avail_in in
	let used_out = out_length - fields.avail_out in
	stream_end, used_in, used_out
);;

let zlib_deflate = make_deflate_or_inflate Zlib.deflate;;

let zlib_inflate = make_deflate_or_inflate Zlib.inflate;;

let crc32 acc s len = Zlib.crc32_substring acc s 0 len;;
