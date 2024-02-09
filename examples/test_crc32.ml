assert (Zlib.crc32_substring 0l "" 0 0 = 0l);;
assert (Zlib.crc32_substring 0l "xx" 1 0 = 0l);;
assert (Zlib.crc32_string 0l "" = 0l);;

assert (Zlib.crc32_substring 0l "ABC" 0 3 = 0xA3830348l);;
assert (Zlib.crc32_substring 0l "xABCx" 1 3 = 0xA3830348l);;
assert (Zlib.crc32_string 0l "ABC" = 0xA3830348l);;

assert (Zlib.crc32_substring 99l "" 0 0 = 99l);;
assert (Zlib.crc32_string 99l "" = 99l);;

Printf.eprintf "ok\n";;
