#include <stdbool.h>
#include <zlib.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

__attribute__((noreturn)) static void zlib_raise(int code)
{
	switch(code){
	case Z_MEM_ERROR:
		caml_raise_out_of_memory();
		break;
	default:
		caml_failwith(zError(code));
	}
}

/* polymorphic variants */

enum {
	Val_ended = -0x1f0b6e0b, /* 0xe0f491f5 */
	Val_ok = 0x0000c239
};

static inline int Flush_val(value v)
{
	switch(v){
	case 0x7dde99cd:
		return Z_NO_FLUSH;
	case -0x423467f3: /* 0xbdcb980d */
		return Z_PARTIAL_FLUSH;
	case -0x0bffcaff: /* 0xf4003501 */
		return Z_SYNC_FLUSH;
	case -0x2362ae97: /* 0xdc9d5169 */
		return Z_FULL_FLUSH;
	case 0x7203d8a7:
		return Z_FINISH;
	case 0x65d55a5b:
		return Z_BLOCK;
	default:
		caml_failwith(__FUNCTION__);
	}
}

static inline int Strategy_val(value v)
{
	switch(v){
	case 0x2ddbee23:
		return Z_DEFAULT_STRATEGY;
	case -0x23e733d1: /* 0xdc18cc2f */
		return Z_FILTERED;
	case -0x36ec11c3: /* 0xc913ee3d */
		return Z_HUFFMAN_ONLY;
	case 0x007cf697:
		return Z_RLE;
	case -0x02df9d57: /* 0xfd2062a9 */
		return Z_FIXED;
	default:
		caml_failwith(__FUNCTION__);
	}
}

/* fields */

static void unset_fields(struct z_stream_s *stream)
{
	stream->next_in = NULL;
	stream->avail_in = 0;
	stream->next_out = NULL;
	stream->avail_out = 0;
}

static void set_fields(struct z_stream_s *stream, value val_fields)
{
	long next_in_offset = Long_val(Field(val_fields, 1));
	stream->next_in = (Bytef *)String_val(Field(val_fields, 0)) + next_in_offset;
	stream->avail_in = Long_val(Field(val_fields, 2));
	long next_out_offset = Long_val(Field(val_fields, 4));
	stream->next_out = (Bytef *)Bytes_val(Field(val_fields, 3)) + next_out_offset;
	stream->avail_out = Long_val(Field(val_fields, 5));
}

static void get_fields(value val_fields, struct z_stream_s const *stream)
{
	long next_in_offset =
		stream->next_in - (Bytef *)String_val(Field(val_fields, 0));
	Store_field(val_fields, 1, Val_long(next_in_offset));
	Store_field(val_fields, 2, Val_long(stream->avail_in));
	long next_out_offset =
		stream->next_out - (Bytef *)Bytes_val(Field(val_fields, 3));
	Store_field(val_fields, 4, Val_long(next_out_offset));
	Store_field(val_fields, 5, Val_long(stream->avail_out));
}

/* custom data */

static inline struct z_stream_s **pZstreams_val(value v)
{
	return (struct z_stream_s **)(Data_custom_val(v));
}

#if defined(SUPPORT_COMPARISON)

static int zstreams_compare(value v1, value v2)
{
	CAMLparam2(v1, v2);
	intptr_t la = (intptr_t)*(pZstreams_val(v1));
	intptr_t ra = (intptr_t)*(pZstreams_val(v2));
	int result = (la < ra)? -1 : (la > ra)? 1 : 0;
	CAMLreturnT(int, result);
}

static long zstreams_hash(value v)
{
	CAMLparam1(v);
	long result = (intptr_t)*(pZstreams_val(v));
	CAMLreturnT(long, result);
}

#endif

/* version functions */

CAMLprim value mlzlib_get_version_string(value val_unit)
{
	CAMLparam1(val_unit);
	CAMLlocal1(val_result);
	val_result = caml_copy_string(zlibVersion());
	CAMLreturn(val_result);
}

/* internal functions */

CAMLprim value mlzlib_ended(value val_stream)
{
	CAMLparam1(val_stream);
	struct z_stream_s *stream = *pZstreams_val(val_stream);
	bool result = stream->zalloc == NULL;
	CAMLreturn(Val_bool(result));
}

/* writer */

static void zstreams_deflate_finalize(value v)
{
	CAMLparam1(v);
	struct z_stream_s *stream = *pZstreams_val(v);
	if(stream != NULL){
		if(stream->zalloc != NULL){
			unset_fields(stream);
			deflateEnd(stream);
		}
		caml_stat_free(stream);
	}
	CAMLreturn0;
}

static struct custom_operations deflate_ops = {
	.identifier = "jp.halfmoon.panathenaia.zlib.deflate",
	.finalize = zstreams_deflate_finalize,
#if defined(SUPPORT_COMPARISON)
	.compare = zstreams_compare,
	.hash = zstreams_hash,
#else
	.compare = custom_compare_default,
	.hash = custom_hash_default,
#endif
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default};

CAMLprim value mlzlib_deflate_init(
	value val_level, value val_strategy, value val_window_bits)
{
	CAMLparam3(val_level, val_strategy, val_window_bits);
	CAMLlocal1(val_result);
	val_result = caml_alloc_custom(&deflate_ops, sizeof(void *), 0, 1);
	struct z_stream_s **pstream = pZstreams_val(val_result);
	*pstream = NULL; /* for the case that caml_stat_alloc fails */
	*pstream = caml_stat_alloc(sizeof(struct z_stream_s));
	struct z_stream_s *stream = *pstream;
	stream->zalloc = NULL;
	stream->zfree = NULL;
	stream->opaque = NULL;
	int err = deflateInit2(
		stream,
		Int_val(val_level),
		Z_DEFLATED,
		Int_val(val_window_bits),
		8,
		Strategy_val(val_strategy));
	if(err != Z_OK) zlib_raise(err);
	/* zalloc is used to determine the valid status. */
	if(stream->zalloc == NULL) caml_failwith(__FUNCTION__);
	CAMLreturn(val_result);
}

CAMLprim value mlzlib_deflate(
	value val_stream, value val_fields, value val_flush)
{
	CAMLparam3(val_stream, val_fields, val_flush);
	CAMLlocal1(val_result);
	struct z_stream_s *stream = *pZstreams_val(val_stream);
	set_fields(stream, val_fields);
	int err = deflate(stream, Flush_val(val_flush));
	get_fields(val_fields, stream);
	switch(err){
	case Z_OK:
		val_result = Val_ok;
		break;
	case Z_STREAM_END:
		val_result = Val_ended;
		break;
	default:
		zlib_raise(err);
	}
	CAMLreturn(val_result);
}

CAMLprim value mlzlib_deflate_end(value val_stream)
{
	CAMLparam1(val_stream);
	struct z_stream_s *stream = *pZstreams_val(val_stream);
	if(stream->zalloc != NULL){
		unset_fields(stream);
		int err = deflateEnd(stream);
		if(err != Z_OK) zlib_raise(err);
		stream->zalloc = NULL;
	}
	CAMLreturn(Val_unit);
}

/* reader */

static void zstreams_inflate_finalize(value v)
{
	CAMLparam1(v);
	struct z_stream_s *stream = *pZstreams_val(v);
	if(stream != NULL){
		if(stream->zalloc != NULL){
			unset_fields(stream);
			inflateEnd(stream);
		}
		caml_stat_free(stream);
	}
	CAMLreturn0;
}

static struct custom_operations inflate_ops = {
	.identifier = "jp.halfmoon.panathenaia.zlib.inflate",
	.finalize = zstreams_inflate_finalize,
#if defined(SUPPORT_COMPARISON)
	.compare = zstreams_compare,
	.hash = zstreams_hash,
#else
	.compare = custom_compare_default,
	.hash = custom_hash_default,
#endif
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default};

CAMLprim value mlzlib_inflate_init(value val_window_bits)
{
	CAMLparam1(val_window_bits);
	CAMLlocal1(val_result);
	val_result = caml_alloc_custom(&inflate_ops, sizeof(void *), 0, 1);
	struct z_stream_s **pstream = pZstreams_val(val_result);
	*pstream = NULL; /* for the case that caml_stat_alloc fails */
	*pstream = caml_stat_alloc(sizeof(struct z_stream_s));
	struct z_stream_s *stream = *pstream;
	stream->zalloc = NULL;
	stream->zfree = NULL;
	stream->opaque = NULL;
	stream->next_in = NULL;
	stream->avail_in = 0;
	int err = inflateInit2(stream, Int_val(val_window_bits));
	if(err != Z_OK) zlib_raise(err);
	/* zalloc is used to determine the valid status. */
	if(stream->zalloc == NULL) caml_failwith(__FUNCTION__);
	CAMLreturn(val_result);
}

CAMLprim value mlzlib_inflate(
	value val_stream, value val_fields, value val_flush)
{
	CAMLparam3(val_stream, val_fields, val_flush);
	CAMLlocal1(val_result);
	struct z_stream_s *stream = *pZstreams_val(val_stream);
	set_fields(stream, val_fields);
	int err = inflate(stream, Flush_val(val_flush));
	get_fields(val_fields, stream);
	switch(err){
	case Z_OK:
		val_result = Val_ok;
		break;
	case Z_STREAM_END:
		val_result = Val_ended;
		break;
	default:
		zlib_raise(err);
	}
	CAMLreturn(val_result);
}

CAMLprim value mlzlib_inflate_end(value val_stream)
{
	CAMLparam1(val_stream);
	struct z_stream_s *stream = *pZstreams_val(val_stream);
	if(stream->zalloc != NULL){
		unset_fields(stream);
		int err = inflateEnd(stream);
		if(err != Z_OK) zlib_raise(err);
		stream->zalloc = NULL;
	}
	CAMLreturn(Val_unit);
}

/* crc32 */

CAMLprim value mlzlib_unsafe_crc32_substring(
	value val_crc, value val_s, value val_pos, value val_len)
{
	CAMLparam4(val_crc, val_s, val_pos, val_len);
	CAMLlocal1(val_result);
	uint32_t r = crc32_z(
		Int32_val(val_crc),
		(Bytef const *)(String_val(val_s) + Long_val(val_pos)),
		Long_val(val_len));
	val_result = caml_copy_int32(r);
	CAMLreturn(val_result);
}
