#include "pbc_nif.h"
#include "pbc_erl.h"
#include "proto.h"

extern ERL_NIF_TERM ATOM_TRUE;
extern ERL_NIF_TERM ATOM_FALSE;
extern ERL_NIF_TERM ATOM_UNDEFINED;
extern FILE* file;

// int32, sint32
int _write_int32(ErlNifEnv* env, void* wmsg, const char *key, ERL_NIF_TERM value)
{
	int number;
	enif_get_int(env, value, &number);
	uint32_t hi = 0;
	if (number < 0)
		hi = ~0;
	return pbc_wmessage_integer(wmsg, key, number, hi);
}

// int64, sint64
int _write_int64(ErlNifEnv* env, void* wmsg, const char *key, ERL_NIF_TERM value)
{
	ErlNifSInt64 number;
	enif_get_int64(env, value, &number);
	uint32_t hi = (uint32_t)(number >> 32);
	return pbc_wmessage_integer(wmsg, key, (uint32_t)number, hi);
}

// uint32, uint64
int _write_uint(ErlNifEnv* env, void* wmsg, const char *key, ERL_NIF_TERM value)
{
	ErlNifUInt64 v;
	enif_get_uint64(env, value, &v);
	uint64_t number = (uint64_t)v;
	uint32_t hi = (uint32_t)(number >> 32);
	return pbc_wmessage_integer(wmsg, key, (uint32_t)number, hi);
}

// float, double
int _write_real(ErlNifEnv* env, void* wmsg, const char *key, ERL_NIF_TERM value)
{
	double number = (double)0;
	int n;
	if (enif_get_double(env, value, &number))
	{
	}
	else
	{
		if (enif_get_int(env, value, &n))
		{
			number = (double)n;
		}
	}
	return pbc_wmessage_real(wmsg, key, number);
}

// fixed32, sfixed32 
int _write_fixed32(ErlNifEnv* env, void* wmsg, const char *key, ERL_NIF_TERM value)
{
	return 0;
}

// fixed64, sfixed64
int _write_fixed64(ErlNifEnv* env, void* wmsg, const char *key, ERL_NIF_TERM value)
{
	return 0;
}

int _write_bool(ErlNifEnv* env, void* wmsg, const char *key, ERL_NIF_TERM value)
{
	int number = 0;
	if (enif_is_atom(env, value))
	{
		number = enif_compare(value, ATOM_TRUE) == 0 ? 1 : 0;
	}
	else if (enif_get_int(env, value, &number))
	{
		number = number ? 1 : 0;
	}
	return pbc_wmessage_integer(wmsg, key, number, 0);
}

int _write_string(ErlNifEnv* env, void* wmsg, const char *key, ERL_NIF_TERM value)
{
	char buf[1024];
	ErlNifBinary bin;
	if (enif_is_list(env, value) && enif_get_string(env, value, buf, sizeof(buf), ERL_NIF_LATIN1))
	{
		return pbc_wmessage_string(wmsg, key, buf, strlen(buf));
	}
	else if (enif_is_binary(env, value) && enif_inspect_binary(env, value, &bin))
	{
		return pbc_wmessage_string(wmsg, key, bin.data, bin.size);
	}
	return 0;
}

int _write_message(ErlNifEnv* env, void* wmsg, const char *key, ERL_NIF_TERM value, const char *type_name)
{
	struct pbc_wmessage *_wmsg = pbc_wmessage_message(wmsg, key);
	return encode_message(env, _wmsg, type_name, value);
}

int _write(ErlNifEnv* env, void* wmsg, const char *key, int pbc_type, ERL_NIF_TERM value, const char *type_name)
{
	int ret;
	switch (pbc_type)
	{
	case PBC_INT:
		ret = _write_int32(env, wmsg, key, value);
		break;
	case PBC_REAL:
		ret = _write_real(env, wmsg, key, value);
		break;
	case PBC_ENUM:
	case PBC_BYTES:
	case PBC_STRING:
		ret = _write_string(env, wmsg, key, value);
		break;
	case PBC_BOOL:
		ret = _write_bool(env, wmsg, key, value);
		break;
	case PBC_UINT:
		ret = _write_uint(env, wmsg, key, value);
		break;
	case PBC_INT64:
		ret = _write_int64(env, wmsg, key, value);
		break;
	case PBC_MESSAGE:
		ret = _write_message(env, wmsg, key, value, type_name);
		break;
	case PBC_FIXED32:
		ret = _write_fixed32(env, wmsg, key, value);
		break;
	case PBC_FIXED64:
		ret = _write_fixed64(env, wmsg, key, value);
		break;
	}
	return ret;
}

int _write_repeated(ErlNifEnv* env, void *wmsg, const char *key, int pbc_type, ERL_NIF_TERM value, const char *type_name)
{
	ERL_NIF_TERM head, tail;
	int ret;
	while (enif_get_list_cell(env, value, &head, &tail))
	{
		ret = _write(env, wmsg, key, pbc_type, head, type_name);
		if (ret < 0) {
			return ret;
		}
		value = tail;
	}
	return 0;
}

int write_field_index(ErlNifEnv* env, void *wmsg, struct _message *m, int index, ERL_NIF_TERM value)
{
	const char *key;
	const char *type;
	int ret;
	int pbc_type = pbc_erl_pbc_type_index(m, index, &key, &type);
	if (pbc_type == PBC_NOEXIST)
	{
		((struct pbc_env*)get_pbc_env())->lasterror = "index not found";
		return -1;
	}
// 	fprintf(file, "encode_field, index:%d, type:%d, key:%s\n", index, pbc_type, key);
// 	fflush(file);
	if (pbc_type & PBC_REPEATED)
	{
		ret = _write_repeated(env, wmsg, key, pbc_type & (~PBC_REPEATED), value, type);
	}
	else
	{
		ret = _write(env, wmsg, key, pbc_type, value, type);
	}
// 	fprintf(file, "encode_field finish\n");
// 	fflush(file);
	return ret;
}

int write_field(ErlNifEnv* env, void *wmsg, struct _message *m, const char *key, ERL_NIF_TERM value)
{
	const char *type;
	int ret;
	int pbc_type = pbc_erl_pbc_type_sp(m, key, &type);
	if (pbc_type == PBC_NOEXIST)
	{
		((struct pbc_env*)get_pbc_env())->lasterror = "key not found";
		return -1;
	}
	if (pbc_type & PBC_REPEATED)
	{
		ret = _write_repeated(env, wmsg, key, pbc_type & (~PBC_REPEATED), value, type);
	}
	else
	{
		ret = _write(env, wmsg, key, pbc_type, value, type);
	}

	return ret;
}

int encode_message(ErlNifEnv* env, void *wmsg, const char *type_name, ERL_NIF_TERM value)
{
	int len, i;
    const ERL_NIF_TERM *array;
	struct _message *m = _pbcP_get_message(get_pbc_env(), type_name);
	int ret;
	if (enif_get_tuple(env, value, &len, &array))
	{
// 		fprintf(file, "encode_record, name: %s, size: %d\n", type_name, len);
// 		fflush(file);
		for (i = 1; i < len; ++i)
		{
			if (enif_compare(array[i], ATOM_UNDEFINED) == 0)
				continue;
			ret = write_field_index(env, wmsg, m, i, array[i]);
			if (ret < 0) return ret;
		}
	}
	else if (enif_is_map(env, value))
	{
		ErlNifMapIterator iter;
		ERL_NIF_TERM k;
		ERL_NIF_TERM v;
		char key[50];
		enif_map_iterator_create(env, value, &iter, ERL_NIF_MAP_ITERATOR_HEAD);
		while (enif_map_iterator_get_pair(env, &iter, &k, &v))
		{
			enif_get_string(env, k, key, sizeof(key), ERL_NIF_LATIN1);
			if (enif_compare(v, ATOM_UNDEFINED) == 0)
				continue;
			write_field(env, wmsg, m, key, v);
			enif_map_iterator_next(env, &iter);
		}
		enif_map_iterator_destroy(env, &iter);
	}
	return 0;

}
