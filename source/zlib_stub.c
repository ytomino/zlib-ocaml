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

static inline struct z_stream_s *zstreams_val(value v)
{
	return (struct z_stream_s *)(Data_custom_val(v));
}

static int zstreams_compare(value v1, value v2)
{
	CAMLparam2(v1, v2);
	intptr_t la = (intptr_t)(Data_custom_val(v1));
	intptr_t ra = (intptr_t)(Data_custom_val(v2));
	int result = (la < ra)? -1 : (la > ra)? 1 : 0;
	CAMLreturn(result);
}

static long zstreams_hash(value v)
{
	CAMLparam1(v);
	long result = (intptr_t)(Data_custom_val(v));
	CAMLreturn(result);
}

/* version functions */

CAMLprim value mlzlib_get_version_string(void)
{
	CAMLparam0();
	CAMLlocal1(val_result);
	val_result = caml_copy_string(zlibVersion());
	CAMLreturn(val_result);
}

/* internal functions */

CAMLprim value mlzlib_avail_in(value val_stream)
{
	CAMLparam1(val_stream);
	struct z_stream_s *stream = zstreams_val(val_stream);
	int result = stream->avail_in;
	CAMLreturn(Val_int(result));
}

CAMLprim value mlzlib_set_in(
	value val_stream, value val_s, value val_pos, value val_len)
{
	CAMLparam4(val_stream, val_s, val_pos, val_len);
	struct z_stream_s *stream = zstreams_val(val_stream);
	stream->next_in = (Bytef *)String_val(val_s) + Long_val(val_pos);
	stream->avail_in = (uInt)Long_val(val_len);
	CAMLreturn(Val_unit);
}

CAMLprim value mlzlib_avail_out(value val_stream)
{
	CAMLparam1(val_stream);
	struct z_stream_s *stream = zstreams_val(val_stream);
	int result = stream->avail_out;
	CAMLreturn(Val_int(result));
}

CAMLprim value mlzlib_set_out(
	value val_stream, value val_s, value val_pos, value val_len)
{
	CAMLparam4(val_stream, val_s, val_pos, val_len);
	struct z_stream_s *stream = zstreams_val(val_stream);
	stream->next_out = (Bytef *)String_val(val_s) + Long_val(val_pos);
	stream->avail_out = (uInt)Long_val(val_len);
	CAMLreturn(Val_unit);
}

CAMLprim value mlzlib_ended(value val_stream)
{
	CAMLparam1(val_stream);
	struct z_stream_s *stream = zstreams_val(val_stream);
	bool result = stream->zalloc == NULL;
	CAMLreturn(Val_bool(result));
}

/* writer */

static void zstreams_deflate_finalize(value v)
{
	CAMLparam1(v);
	struct z_stream_s *stream = zstreams_val(v);
	if(stream->zalloc != NULL){
		deflateEnd(stream);
	}
	CAMLreturn0;
}

static struct custom_operations deflate_ops = {
	.identifier = "jp.halfmoon.panathenaia.zlib",
	.finalize = zstreams_deflate_finalize,
	.compare = zstreams_compare,
	.hash = zstreams_hash,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default};

CAMLprim value mlzlib_deflate_init(
	value val_level, value val_strategy, value val_window_bits)
{
	CAMLparam3(val_level, val_strategy, val_window_bits);
	CAMLlocal1(val_result);
	val_result = caml_alloc_custom(&deflate_ops, sizeof(struct z_stream_s), 0, 1);
	struct z_stream_s *stream = zstreams_val(val_result);
	stream->zalloc = NULL;
	stream->zfree = NULL;
	stream->opaque = NULL;
	int err = deflateInit2(
		stream,
		Int_val(val_level),
		Z_DEFLATED,
		Int_val(val_window_bits),
		8,
		Int_val(val_strategy));
	if(err != Z_OK) zlib_raise(err);
	/* zalloc is used to determine the valid status. */
	if(stream->zalloc == NULL) caml_failwith(__FUNCTION__);
	CAMLreturn(val_result);
}

CAMLprim value mlzlib_deflate(value val_stream, value val_flush)
{
	CAMLparam2(val_stream, val_flush);
	struct z_stream_s *stream = zstreams_val(val_stream);
	int err = deflate(stream, Int_val(val_flush));
	bool result;
	switch(err){
	case Z_OK:
		result = false;
		break;
	case Z_STREAM_END:
		result = true;
		break;
	default:
		zlib_raise(err);
	}
	CAMLreturn(Val_bool(result));
}

CAMLprim value mlzlib_deflate_end(value val_stream)
{
	CAMLparam1(val_stream);
	struct z_stream_s *stream = zstreams_val(val_stream);
	if(stream->zalloc != NULL){
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
	struct z_stream_s *stream = zstreams_val(v);
	if(stream->zalloc != NULL){
		inflateEnd(stream);
	}
	CAMLreturn0;
}

static struct custom_operations inflate_ops = {
	.identifier = "jp.halfmoon.panathenaia.zlib",
	.finalize = zstreams_inflate_finalize,
	.compare = zstreams_compare,
	.hash = zstreams_hash,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default};

CAMLprim value mlzlib_inflate_init(value val_window_bits)
{
	CAMLparam1(val_window_bits);
	CAMLlocal1(val_result);
	val_result = caml_alloc_custom(&inflate_ops, sizeof(struct z_stream_s), 0, 1);
	struct z_stream_s *stream = zstreams_val(val_result);
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

CAMLprim value mlzlib_inflate(value val_stream, value val_flush)
{
	CAMLparam2(val_stream, val_flush);
	struct z_stream_s *stream = zstreams_val(val_stream);
	int err = inflate(stream, Int_val(val_flush));
	bool result;
	switch(err){
	case Z_OK:
		result = false;
		break;
	case Z_STREAM_END:
		result = true;
		break;
	default:
		zlib_raise(err);
	}
	CAMLreturn(Val_bool(result));
}

CAMLprim value mlzlib_inflate_end(value val_stream)
{
	CAMLparam1(val_stream);
	struct z_stream_s *stream = zstreams_val(val_stream);
	if(stream->zalloc != NULL){
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
