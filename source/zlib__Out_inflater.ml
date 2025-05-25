open Zlib;;

external closed: z_stream_inflate -> bool = "mlzlib_closed";;

type t =
	z_stream_inflate * z_fields * bool ref * (string -> int -> int -> unit);;

let open_out ?(header: [z_header | `auto] = `auto)
	(output: string -> int -> int -> unit) =
(
	let stream = inflate_init ~header () in
	stream, _init_fields_out (), ref false, output
);;

let unsafe_out_substring = _make_out inflate;;

let out_substring (oi: t) (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && len <= String.length s - pos
	then unsafe_out_substring oi s pos len
	else invalid_arg "Zlib.Out_inflater.out_substring" (* __FUNCTION__ *)
);;

let flush = _flush;;

let close_out = _make_close_out inflate inflate_close closed;;

let is_inflated_out (oi: t) = (
	let _, _, stream_end_ref, _ = oi in
	!stream_end_ref
);;
