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

static inline struct z_stream_s *zstreams_val(value data)
{
	return (struct z_stream_s *)(Data_custom_val(data));
}

static int zstreams_compare(value left, value right)
{
	CAMLparam2(left, right);
	intptr_t la = (intptr_t)(Data_custom_val(left));
	intptr_t ra = (intptr_t)(Data_custom_val(right));
	int result = (la < ra)? -1 : (la > ra)? 1 : 0;
	CAMLreturn(result);
}

static long zstreams_hash(value data)
{
	CAMLparam1(data);
	long result = (intptr_t)(Data_custom_val(data));
	CAMLreturn(result);
}

/* version functions */

CAMLprim value mlzlib_get_version_string(void)
{
	CAMLparam0();
	CAMLlocal1(result);
	result = caml_copy_string(zlibVersion());
	CAMLreturn(result);
}

/* internal functions */

CAMLprim value mlzlib_avail_in(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
	int result = stream->avail_in;
	CAMLreturn(Val_int(result));
}

CAMLprim value mlzlib_set_in(value data, value s, value pos, value len)
{
	CAMLparam4(data, s, pos, len);
	struct z_stream_s *stream = zstreams_val(data);
	stream->next_in = (Bytef *)String_val(s) + Int_val(pos);
	stream->avail_in = Int_val(len);
	CAMLreturn(Val_unit);
}

CAMLprim value mlzlib_avail_out(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
	int result = stream->avail_out;
	CAMLreturn(Val_int(result));
}

CAMLprim value mlzlib_set_out(value data, value s, value pos, value len)
{
	CAMLparam4(data, s, pos, len);
	struct z_stream_s *stream = zstreams_val(data);
	stream->next_out = (Bytef *)String_val(s) + Int_val(pos);
	stream->avail_out = Int_val(len);
	CAMLreturn(Val_unit);
}

CAMLprim value mlzlib_ended(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
	bool result = stream->zalloc == NULL;
	CAMLreturn(Val_bool(result));
}

/* writer */

static void zstreams_deflate_finalize(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
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

CAMLprim value mlzlib_deflate_init(value level, value strategy,
	value window_bits)
{
	CAMLparam3(level, strategy, window_bits);
	CAMLlocal1(result);
	result = caml_alloc_custom(&deflate_ops, sizeof(struct z_stream_s), 0, 1);
	struct z_stream_s *stream = zstreams_val(result);
	stream->zalloc = NULL;
	stream->zfree = NULL;
	stream->opaque = NULL;
	int err = deflateInit2(stream,
		Int_val(level),
		Z_DEFLATED,
		Int_val(window_bits),
		8,
		Int_val(strategy));
	if(err != Z_OK) zlib_raise(err);
	/* zalloc is used to determine the valid status. */
	if(stream->zalloc == NULL) caml_failwith(__FUNCTION__);
	CAMLreturn(result);
}

CAMLprim value mlzlib_deflate(value data, value flush_value)
{
	CAMLparam2(data, flush_value);
	struct z_stream_s *stream = zstreams_val(data);
	int err = deflate(stream, Int_val(flush_value));
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

CAMLprim value mlzlib_deflate_end(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
	if(stream->zalloc != NULL){
		int err = deflateEnd(stream);
		if(err != Z_OK) zlib_raise(err);
		stream->zalloc = NULL;
	}
	CAMLreturn(Val_unit);
}

/* reader */

static void zstreams_inflate_finalize(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
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

CAMLprim value mlzlib_inflate_init(value window_bits)
{
	CAMLparam1(window_bits);
	CAMLlocal1(result);
	result = caml_alloc_custom(&inflate_ops, sizeof(struct z_stream_s), 0, 1);
	struct z_stream_s *stream = zstreams_val(result);
	stream->zalloc = NULL;
	stream->zfree = NULL;
	stream->opaque = NULL;
	stream->next_in = NULL;
	stream->avail_in = 0;
	int err = inflateInit2(stream, Int_val(window_bits));
	if(err != Z_OK) zlib_raise(err);
	/* zalloc is used to determine the valid status. */
	if(stream->zalloc == NULL) caml_failwith(__FUNCTION__);
	CAMLreturn(result);
}

CAMLprim value mlzlib_inflate(value data, value flush_value)
{
	CAMLparam2(data, flush_value);
	struct z_stream_s *stream = zstreams_val(data);
	int err = inflate(stream, Int_val(flush_value));
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

CAMLprim value mlzlib_inflate_end(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
	if(stream->zalloc != NULL){
		int err = inflateEnd(stream);
		if(err != Z_OK) zlib_raise(err);
		stream->zalloc = NULL;
	}
	CAMLreturn(Val_unit);
}

/* crc32 */

CAMLprim value mlzlib_crc32_substring(value crc, value s, value pos, value len)
{
	CAMLparam4(crc, s, pos, len);
	CAMLlocal1(result);
	uint32_t r = crc32_z(
		Int32_val(crc),
		(Bytef const *)(String_val(s) + Long_val(pos)),
		Long_val(len));
	result = caml_copy_int32(r);
	CAMLreturn(result);
}
