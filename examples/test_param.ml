(* Zlib.z_flush *)

let check_flush flush = (
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
	let result =
		try
			let _: [`ended | `ok] = Zlib.deflate d fields flush in
			true
		with
		| Failure msg -> false
	in
	begin try Zlib.deflate_close d with
	| Failure msg as exn ->
		(* ignore that some input or output was discarded. *)
		if msg <> "data error" then (
			raise exn
		)
	end;
	result
) in
assert (check_flush `NO_FLUSH);
assert (check_flush `PARTIAL_FLUSH);
assert (check_flush `SYNC_FLUSH);
assert (check_flush `FULL_FLUSH);
assert (check_flush `FINISH);
assert (check_flush `BLOCK);;

let check_flush_inflate flush = (
	let i = Zlib.inflate_init ~header:`default () in
	let input = "x\x9cs\x04\x00\x00B\x00B" in (* compressed "A" *)
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
	let result =
		try
			let _: [`ended | `ok] = Zlib.inflate i fields flush in
			true
		with
		| Failure msg -> false
	in
	Zlib.inflate_close i;
	result
) in
assert (check_flush_inflate `TREES);;

(* Zlib.z_strategy *)

let check_strategy strategy = (
	try
		Zlib.deflate_end_out (Zlib.deflate_init_out ~strategy (fun _ _ _ -> ()));
		true
	with
	| Failure _ -> false
) in
assert (check_strategy `DEFAULT_STRATEGY);
assert (check_strategy `FILTERED);
assert (check_strategy `HUFFMAN_ONLY);
assert (check_strategy `RLE);
assert (check_strategy `FIXED);;

prerr_endline "ok";;
