let verbose = Array.length Sys.argv > 1 && Sys.argv.(1) = "--verbose";;

let read data cursor dest index length = (
	let really_length = (
		if !cursor + length <= String.length data then length else
		String.length data - !cursor
	) in
	Bytes.blit_string data !cursor dest index really_length;
	cursor := !cursor + really_length;
	really_length
);;

let read_file filename = (
	let f = open_in_bin filename in
	let s = Bytes.create (in_channel_length f) in
	really_input f s 0 (Bytes.length s);
	close_in f;
	Bytes.unsafe_to_string s
);;

let src = read_file "test_gz.ml";; (* this file *)

let gz = (
	let buf = Buffer.create 1024 in
	let w = Zlib.deflate_init_out (Buffer.add_substring buf) in
	Zlib.deflate_out w src 0 (String.length src);
	Zlib.deflate_end_out w;
	Buffer.contents buf
);;

if verbose then (
	Printf.printf "%d/%d\n" (String.length gz) (String.length src);
	flush stdout
);;

let dest = (
	let buf = Buffer.create 1024 in
	let r = Zlib.inflate_init_in (read gz (ref 0)) in
	while
		let s = Bytes.create 1024 in
		let ri = Zlib.inflate_in r s 0 1024 in
		Buffer.add_subbytes buf s 0 ri;
		ri > 0
	do () done;
	Zlib.inflate_end_in r;
	Buffer.contents buf
);;

assert (src = dest);;

let gz = (
	let buf = Buffer.create 1024 in
	let w = Zlib.deflate_init_out ~header:`gzip (Buffer.add_substring buf) in
	Zlib.deflate_out w src 0 (String.length src);
	Zlib.deflate_end_out w;
	Buffer.contents buf
);;

let out_gz_name = Filename.temp_file "test-out" ".gz" in
let out_txt_name = Filename.temp_file "test-out" ".txt" in
let f = open_out_bin out_gz_name in
output_string f gz;
close_out f;
ignore (
	Sys.command ("gzip -c -d '" ^ out_gz_name ^ "' > '" ^ out_txt_name ^ "'"));
let unzip = read_file out_txt_name in
assert (unzip = src);
Sys.remove out_gz_name;
Sys.remove out_txt_name;;

(* report *)

Printf.eprintf "ok\n";;
