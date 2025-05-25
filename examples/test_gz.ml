let verbose = Array.length Sys.argv > 1 && Sys.argv.(1) = "--verbose";;

let read data cursor dest index length = (
	let really_length = (
		let r = String.length data - !cursor in
		if length <= r then length
		else r
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

(* out_deflator *)

let gz = (
	let buf = Buffer.create 1024 in
	let w = Zlib.Out_deflater.open_out (Buffer.add_substring buf) in
	Zlib.Out_deflater.output_string w src;
	Zlib.Out_deflater.close_out w;
	Buffer.contents buf
);;

if verbose then (
	Printf.eprintf "%d/%d\n" (String.length gz) (String.length src);
	flush stderr
);;

(* in_inflater *)

let dest = (
	let buf = Buffer.create 1024 in
	let r = Zlib.In_inflater.open_in (read gz (ref 0)) in
	while
		let s = Bytes.create 1024 in
		let ri = Zlib.In_inflater.input r s 0 1024 in
		Buffer.add_subbytes buf s 0 ri;
		ri > 0
	do () done;
	Zlib.In_inflater.close_in r;
	Buffer.contents buf
) in
assert (src = dest);;

(* out_inflater *)

let dest = (
	let buf = Buffer.create 1024 in
	let w = Zlib.Out_inflater.open_out (Buffer.add_substring buf) in
	let used = Zlib.Out_inflater.out_substring w gz 0 (String.length gz) in
	assert (used = String.length gz);
	Zlib.Out_inflater.close_out w;
	Buffer.contents buf
) in
assert (src = dest);;

(* gzip *)

let gz = (
	let buf = Buffer.create 1024 in
	let w = Zlib.Out_deflater.open_out ~header:`gzip (Buffer.add_substring buf) in
	Zlib.Out_deflater.output_substring w src 0 (String.length src);
	Zlib.Out_deflater.close_out w;
	Buffer.contents buf
);;

let out_gz_name = Filename.temp_file "test-out" ".gz" in
let out_txt_name = Filename.temp_file "test-out" ".txt" in
let f = open_out_bin out_gz_name in
output_string f gz;
close_out f;
let _: int =
	Sys.command ("gzip -c -d '" ^ out_gz_name ^ "' > '" ^ out_txt_name ^ "'")
in
let unzip = read_file out_txt_name in
assert (unzip = src);
Sys.remove out_gz_name;
Sys.remove out_txt_name;;

(* report *)

prerr_endline "ok";;
