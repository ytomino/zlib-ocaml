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
