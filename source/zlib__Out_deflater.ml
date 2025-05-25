open Zlib;;
open Zlib_internalOut;;

external closed: z_stream_deflate -> bool = "mlzlib_closed";;

type t =
	z_stream_deflate * z_fields * bool ref * (string -> int -> int -> unit);;

let open_out ?(level: int = z_default_compression)
	?(strategy: z_strategy = `DEFAULT_STRATEGY) ?(header: z_header = `default)
	(output: string -> int -> int -> unit) =
(
	let stream = deflate_init ~level ~strategy ~header () in
	stream, _init_fields_out (), ref false, output
);;

let unsafe_out_substring = _make_out deflate;;

let out_substring (od: t) (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && len <= String.length s - pos
	then unsafe_out_substring od s pos len
	else invalid_arg "Zlib.Out_deflater.out_substring" (* __FUNCTION__ *)
);;

let output_substring (od: t) (s: string) (pos: int) (len: int) = (
	let loc = "Zlib.Out_deflater.output_substring" (* __FUNCTION__ *) in
	if pos >= 0 && len >= 0 && len <= String.length s - pos then (
		let r = unsafe_out_substring od s pos len in
		if r <> len then failwith loc
	) else invalid_arg loc
);;

let output_string (od: t) (s: string) = (
	let len = String.length s in
	let r = unsafe_out_substring od s 0 len in
	if r <> len then failwith "Zlib.Out_deflater.output_string" (* __FUNCTION__ *)
);;

let flush = _flush;;

let close_out = _make_close_out deflate deflate_close closed;;
