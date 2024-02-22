let max_wbits = 15;;

type z_level = Z_DEFAULT_COMPRESSION;;

let zlib_deflateInit level = (
	let level =
		match level with
		| Z_DEFAULT_COMPRESSION -> Zlib.z_default_compression
	in
	let buffer = Buffer.create 0 in
	Zlib.deflate_init_out ~level (Buffer.add_substring buffer) ~header:`gzip,
	buffer,
	ref false
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
	let buffer = Buffer.create 0 in
	Zlib.inflate_init_out ~header (Buffer.add_substring buffer), buffer, ref false
);;

let zlib_inflateInit () = zlib_inflateInit2 47;;

let make_deflate_or_inflate_end end_f (writer, (_: Buffer.t), stream_end) = (
	if not !stream_end then end_f writer
);;

let zlib_deflateEnd = make_deflate_or_inflate_end Zlib.deflate_end_out;;

let zlib_inflateEnd = make_deflate_or_inflate_end Zlib.inflate_end_out;;

type z_flush = Z_NO_FLUSH | Z_SYNC_FLUSH | Z_FINISH;;

let make_deflate_or_inflate out_f flush_f end_f completed_f
	(writer, buffer, stream_end)
	(flush: z_flush) in_s in_offset in_length out_s out_offset out_length =
(
	let used_in =
		if !stream_end then 0 else
		let used_in = out_f writer in_s in_offset in_length in
		if used_in = 0 && (flush = Z_FINISH || completed_f writer) then (
			stream_end := true;
			end_f writer
		) else flush_f writer;
		used_in
	in
	let used_out = min (Buffer.length buffer) out_length in
	Buffer.blit buffer 0 out_s out_offset used_out;
	let rest_length = Buffer.length buffer - used_out in
	let rest = Buffer.sub buffer used_out rest_length in
	Buffer.clear buffer;
	Buffer.add_string buffer rest;
	!stream_end && rest_length = 0, used_in, used_out
);;

let zlib_deflate =
	make_deflate_or_inflate Zlib.deflate_out Zlib.deflate_flush
		Zlib.deflate_end_out (fun _ -> false);;

let zlib_inflate =
	make_deflate_or_inflate Zlib.inflate_out Zlib.inflate_flush
		Zlib.inflate_end_out Zlib.is_inflated_out;;

let crc32 acc s len = Zlib.crc32_substring acc s 0 len;;
