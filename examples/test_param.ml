(* Zlib.strategy *)

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
