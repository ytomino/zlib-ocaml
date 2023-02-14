#include <zlib.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

static void zlib_raise(int code)
{
	switch(code){
	case Z_MEM_ERROR:
		raise_out_of_memory();
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

/* writer */

static void zstreams_deflate_finalize(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
	deflateEnd(stream);
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
	result = alloc_custom(&deflate_ops, sizeof(struct z_stream_s), 0, 1);
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
	CAMLreturn(result);
}

CAMLprim value mlzlib_deflate(value data, value flush)
{
	CAMLparam2(data, flush);
	CAMLlocal1(result);
	struct z_stream_s *stream = zstreams_val(data);
	int err = deflate(stream, Int_val(flush));
	switch(err){
	case Z_OK:
		result = Val_false;
		break;
	case Z_STREAM_END:
		result = Val_true;
		break;
	default:
		zlib_raise(err);
	}
	CAMLreturn(result);
}

CAMLprim value mlzlib_deflate_end(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
	int err = deflateEnd(stream);
	if(err != Z_OK) zlib_raise(err);
	CAMLreturn(Val_unit);
}

/* reader */

static void zstreams_inflate_finalize(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
	inflateEnd(stream);
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
	result = alloc_custom(&inflate_ops, sizeof(struct z_stream_s), 0, 1);
	struct z_stream_s *stream = zstreams_val(result);
	stream->zalloc = NULL;
	stream->zfree = NULL;
	stream->opaque = NULL;
	int err = inflateInit2(stream, Int_val(window_bits));
	if(err != Z_OK) zlib_raise(err);
	CAMLreturn(result);
}

CAMLprim value mlzlib_inflate(value data, value flush)
{
	CAMLparam2(data, flush);
	CAMLlocal1(result);
	struct z_stream_s *stream = zstreams_val(data);
	int err = inflate(stream, Int_val(flush));
	switch(err){
	case Z_OK:
		result = Val_false;
		break;
	case Z_STREAM_END:
		result = Val_true;
		break;
	default:
		zlib_raise(err);
	}
	CAMLreturn(result);
}

CAMLprim value mlzlib_inflate_end(value data)
{
	CAMLparam1(data);
	struct z_stream_s *stream = zstreams_val(data);
	int err = inflateEnd(stream);
	if(err != Z_OK) zlib_raise(err);
	CAMLreturn(Val_unit);
}
