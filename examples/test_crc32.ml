assert (Zlib.crc32_substring 0l "" 0 0 = 0l);;
assert (Zlib.crc32_substring 0l "xx" 1 0 = 0l);;

assert (Zlib.crc32_substring 0l "ABC" 0 3 = 0xA3830348l);;
assert (Zlib.crc32_substring 0l "xABCx" 1 3 = 0xA3830348l);;

Printf.eprintf "ok\n";;
