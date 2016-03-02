#include "pbc_nif.h"
#include "pbc_erl.h"
#include "proto.h"
#include <stdlib.h>

extern FILE* file;
extern ERL_NIF_TERM default_records;
extern ERL_NIF_TERM default_maps;
extern ERL_NIF_TERM ATOM_UNDEFINED;
extern ERL_NIF_TERM ATOM_TRUE;
extern ERL_NIF_TERM ATOM_FALSE;

struct decode_env
{
	ErlNifEnv* env;
	int decode_type;
	ERL_NIF_TERM* array;
	int           arity;
	ERL_NIF_TERM  map;
};

int get_default_record(ErlNifEnv* env, const char* type_name, ERL_NIF_TERM* term)
{
	ERL_NIF_TERM key = enif_make_string(env, type_name, ERL_NIF_LATIN1);
	return enif_get_map_value(env, default_records, key, term);
}

int get_default_map(ErlNifEnv* env, const char* type_name, ERL_NIF_TERM* default_map)
{
	ERL_NIF_TERM key = enif_make_string(env, type_name, ERL_NIF_LATIN1);
	return enif_get_map_value(env, default_maps, key, default_map);
}


int _read(ErlNifEnv* env, int pbc_type, const char *type_name, union pbc_value *v, ERL_NIF_TERM* result, int decode_type)
{
	uint64_t v64;
	struct pbc_slice slice;
	switch (pbc_type){
	case PBC_INT:
		*result = enif_make_int(env, (int)v->i.low);
		break;
	case PBC_REAL:
		*result = enif_make_double(env, v->f);
		break;
	case PBC_BOOL:
		*result = v->i.low ? ATOM_TRUE : ATOM_FALSE;
		break;
	case PBC_ENUM:
		*result = enif_make_int(env, v->e.id);
		break;
	case PBC_BYTES:
	case PBC_STRING:
		*result = enif_make_string_len(env, (const char*)v->s.buffer, v->s.len, ERL_NIF_LATIN1);
		break;
	case PBC_INT64:
		v64 = (uint64_t)(v->i.hi) << 32 | (uint64_t)(v->i.low);
		*result = enif_make_int64(env, (ErlNifSInt64)v64);
		break;
	case PBC_UINT:
		v64 = (uint64_t)(v->i.hi) << 32 | (uint64_t)(v->i.low);
		*result = enif_make_uint64(env, (ErlNifUInt64)v64);
		break;
	case PBC_MESSAGE:
		slice.buffer = (char*)v->s.buffer;
		slice.len = v->s.len;
		decode_message(env, type_name, &slice, result, decode_type);
		break;
	default:
		*result = ATOM_UNDEFINED;
		break;
	}
	return 0;
}

void decode_cb(void *ud, int pbc_type, const char * type_name, union pbc_value *v, int id, const char *key)
{
	struct decode_env* user_data = (struct decode_env*)ud;
	ErlNifEnv* env = user_data->env;
	int decode_type = user_data->decode_type;
	ERL_NIF_TERM key_term;
	ERL_NIF_TERM value;
	if (key == NULL || pbc_type == PBC_NOEXIST) return;
	if (pbc_type & PBC_REPEATED)
	{
		ERL_NIF_TERM cell;
		_read(env, pbc_type & ~PBC_REPEATED, type_name, v, &cell, decode_type);
		if (decode_type == decode_type_record)
		{
			if (enif_is_list(env, user_data->array[id]))
			{
				user_data->array[id] = enif_make_list_cell(env, cell, user_data->array[id]);
			}
		}
		else
		{
			key_term = enif_make_string(env, key, ERL_NIF_LATIN1);
			enif_get_map_value(env, user_data->map, key_term, &value);
			if (enif_is_list(env, value))
			{
				value = enif_make_list_cell(env, cell, value);
				enif_make_map_update(env, user_data->map, key_term, value, &user_data->map);
			}
		}
	}
	else
	{
		if (decode_type == decode_type_record)
		{
			_read(env, pbc_type, type_name, v, &user_data->array[id], decode_type);
		}
		else
		{
			key_term = enif_make_string(env, key, ERL_NIF_LATIN1);
			_read(env, pbc_type, type_name, v, &value, decode_type);
			enif_make_map_update(env, user_data->map, key_term, value, &user_data->map);
		}
	}
}

int decode_message(ErlNifEnv* env, const char* type_name, struct pbc_slice* slice, ERL_NIF_TERM* result, int decode_type)
{
	struct decode_env user_data;
	user_data.env = env;
	user_data.decode_type = decode_type;
	ERL_NIF_TERM term;
	int ret;
	if (decode_type == decode_type_record && get_default_record(enif_priv_data(env), type_name, &term))
	{
		ERL_NIF_TERM record = enif_make_copy(env, term);
		enif_get_tuple(env, record, &user_data.arity, (const ERL_NIF_TERM **)&user_data.array);
		ret = pbc_decode(get_pbc_env(), type_name, slice, decode_cb, &user_data);
		*result = enif_make_tuple_from_array(env, user_data.array, user_data.arity);
	}
	else if (decode_type == decode_type_map && get_default_map(enif_priv_data(env), type_name, &term))
	{
		user_data.map = enif_make_copy(env, term);
		ret = pbc_decode(get_pbc_env(), type_name, slice, decode_cb, &user_data);
		*result = user_data.map;
	}
	return ret;
}

int make_default_term(ErlNifEnv* env, struct _message* m, ERL_NIF_TERM* default_record, ERL_NIF_TERM* default_map)
{
	ERL_NIF_TERM key_term1, key_term2, record, map, map1;
	ERL_NIF_TERM* array;
    int i;
	int array_size;
	int field_count = pbc_erl_field_count(m);
	//fprintf(file, "make_default_term, type_name: %s, field_count: %d\n", m->key, field_count);
	//fflush(file);
	array_size = field_count + 1;
	array = enif_alloc(sizeof(ERL_NIF_TERM)* array_size);
	for (i = 0; i < array_size; i++)
	{
		array[i] = ATOM_UNDEFINED;
	}
	array[0] = enif_make_atom(env, m->key);
	map = enif_make_new_map(env);
	for (i = 1; i < array_size; i++)
	{
		const char *key = NULL;
		const char *type_name = NULL;
		int pbc_type = pbc_erl_pbc_type_ip(m, i, &key, &type_name);
		if (pbc_type < 0) continue;
		//fprintf(file, "pbc_type: %d, id: %d, key: %s\n", pbc_type, i, key);
		//fflush(file);
		key_term1 = enif_make_string(env, key, ERL_NIF_LATIN1);
		if (pbc_type & PBC_REPEATED)
		{
			array[i] = enif_make_list(env, 0);
			enif_make_map_put(env, map, key_term1, array[i], &map);
		}
		else if (pbc_type == PBC_MESSAGE)
		{
			struct _message* m1 = _pbcP_get_message(get_pbc_env(), type_name);
			key_term2 = enif_make_string(env, m1->key, ERL_NIF_LATIN1);
			get_default_map(env, type_name, &map1);
			if (!get_default_record(env, type_name, &record))
			{
				make_default_term(env, m1, &record, &map1);
				enif_make_map_put(env, default_records, key_term2, record, &default_records);
				enif_make_map_put(env, default_maps, key_term2, map1, &default_maps);
			}
			array[i] = record;
			enif_make_map_put(env, map, key_term1, map1, &map);
		}
		else
		{
			pbc_var defv;
			union pbc_value* pbc_v;
			_pbcP_message_default(m, key, defv);
			pbc_v = (union pbc_value*)defv;
			_read(env, pbc_type, type_name, pbc_v, &array[i], decode_type_record);
			enif_make_map_put(env, map, key_term1, array[i], &map);
		}
	}
	record = enif_make_tuple_from_array(env, array, array_size);
	*default_record = record;
	*default_map = map;
	enif_free(array);
	return 1;
}

void iterator_msgs_cb(void *p, void *ud)
{
	struct _message* m = (struct _message*)p;
	ErlNifEnv* env = (ErlNifEnv*)ud;
	ERL_NIF_TERM key;
	ERL_NIF_TERM record;
	ERL_NIF_TERM map;
	if (!get_default_record(env, m->key, &record))
	{
		make_default_term(env, m, &record, &map);
		key = enif_make_string(env, m->key, ERL_NIF_LATIN1);
		enif_make_map_put(env, default_records, key, record, &default_records);
		enif_make_map_put(env, default_maps, key, map, &default_maps);
	}
}

int init_default(ErlNifEnv* env)
{
	if (default_records != 0) return 0;
	struct pbc_env* ppbc_env = get_pbc_env();
	default_records = enif_make_new_map(env);
	default_maps = enif_make_new_map(env);
	//fprintf(file, "ppbc_env: %d\n", ppbc_env);
	fflush(file);
	_pbcM_sp_foreach_ud(ppbc_env->msgs, iterator_msgs_cb, env);
	ErlNifEnv* env_priv = enif_priv_data(env);
	enif_clear_env(env_priv);
	default_records = enif_make_copy(env_priv, default_records);
	default_maps = enif_make_copy(env_priv, default_maps);
	return 0;
}
