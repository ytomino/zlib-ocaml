(* z_stream_deflate *)

let d =
	Zlib.deflate_init ~level:Zlib.z_default_compression ~strategy:`DEFAULT_STRATEGY
		~header:`default ()
in
Zlib.deflate_close d;
Zlib.deflate_close d;;

let d =
	Zlib.deflate_init ~level:Zlib.z_default_compression ~strategy:`DEFAULT_STRATEGY
		~header:`default ()
in
let input = "A" in
let output = Bytes.create 4096 in
let fields = {
	Zlib.next_in = input;
	next_in_offset = 0;
	avail_in = String.length input;
	next_out = output;
	next_out_offset = 0;
	avail_out = Bytes.length output
}
in
begin match Zlib.deflate d fields `NO_FLUSH with
| `ended -> assert false
| `ok -> ()
end;
begin match Zlib.deflate_close d with
| () ->
	assert false
| exception (Failure msg as exn) ->
	if msg <> "data error" then (
		raise exn
	)
end;
Zlib.deflate_close d;;

(* z_stream_inflate *)

let i = Zlib.inflate_init ~header:`default () in
Zlib.inflate_close i;
Zlib.inflate_close i;;

(* out_deflator *)

let w = Zlib.deflate_init_out (fun _ _ _ -> ()) in
Zlib.deflate_end_out w;
Zlib.deflate_end_out w;; (* accepts multiple calling _end_ *)

(* in_inflater *)

let r = Zlib.inflate_init_in (fun _ _ _ -> 0) in
Zlib.inflate_end_in r;
Zlib.inflate_end_in r;;

(* out_inflater *)

let w = Zlib.inflate_init_out (fun _ _ _ -> ()) in
begin match Zlib.inflate_end_out w with
| () -> assert false (* should be failure because null data is invalid *)
| exception Failure _ -> () (* "buffer error" *)
end;
Zlib.inflate_end_out w;; (* no exception because it has already been ended *)

let compressed_null = "\x78\x9c\x03\x00\x00\x00\x00\x01";;

let w = Zlib.inflate_init_out (fun _ _ _ -> ()) in
let compressed_null_length = String.length compressed_null in
let used = Zlib.inflate_out w compressed_null 0 compressed_null_length in
assert (used = compressed_null_length);
Zlib.inflate_end_out w;
Zlib.inflate_end_out w;;

(* report *)

prerr_endline "ok";;
