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
	let w = Zlib.deflate_init (Buffer.add_substring buf) in
	Zlib.deflate w src 0 (String.length src);
	Zlib.deflate_end w;
	Buffer.contents buf
);;

if verbose then (
	Printf.printf "%d/%d\n" (String.length gz) (String.length src);
	flush stdout
);;

let dest = (
	let buf = Buffer.create 1024 in
	let r = Zlib.inflate_init (read gz (ref 0)) in
	while
		let s = Bytes.create 1024 in
		let ri = Zlib.inflate r s 0 1024 in
		Buffer.add_subbytes buf s 0 ri;
		ri > 0
	do () done;
	Zlib.inflate_end r;
	Buffer.contents buf
);;

assert (src = dest);;

let gz = (
	let buf = Buffer.create 1024 in
	let w = Zlib.deflate_init ~header:`gzip (Buffer.add_substring buf) in
	Zlib.deflate w src 0 (String.length src);
	Zlib.deflate_end w;
	Buffer.contents buf
);;

let f = open_out_bin "test-out.gz" in
output_string f gz;
close_out f;
ignore (Sys.command "gzip -c -d test-out.gz > test-out.txt");
let unzip = read_file "test-out.txt" in
assert (unzip = src);
Sys.remove "test-out.gz";
Sys.remove "test-out.txt";;

(* report *)

Printf.eprintf "ok\n";;
