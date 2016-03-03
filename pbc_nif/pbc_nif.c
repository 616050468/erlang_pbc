#include "pbc_nif.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>

//__declspec(thread) static struct pbc_env* ppbc_env = NULL;
//static struct pbc_env* ppbc_env = NULL;
FILE* file;
ERL_NIF_TERM default_records = 0;
ERL_NIF_TERM default_maps = 0;

ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_UNDEFINED;

void read_file(const char *filename, struct pbc_slice *slice);

ERL_NIF_TERM nif_register(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_get_default(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_all_default(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
	{ "register", 1, nif_register },
	{ "encode",  2, nif_encode },
	{ "decode", 3, nif_decode },
	{ "get_default", 2, nif_get_default },
	{ "all_default", 1, nif_all_default },
};

ERL_NIF_TERM nif_register(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct pbc_env* ppbc_env = get_pbc_env();
	if (ppbc_env != NULL)
	{
		return ATOM_OK;
	}
	ppbc_env = pbc_new();
    set_pbc_env(ppbc_env);
	char name[100];
	enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1);
	struct pbc_slice slice;
	read_file(name, &slice);
	int ret = pbc_register(ppbc_env, &slice);
	assert(ret == 0);
	init_default(env);
	return ATOM_OK;
}

ERL_NIF_TERM nif_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct pbc_env* ppbc_env = get_pbc_env();
	if (ppbc_env == NULL)
	{
		return ATOM_ERROR;
	}
	char type_name[100];
	enif_get_string(env, argv[0], type_name, sizeof(type_name), ERL_NIF_LATIN1);
	struct pbc_wmessage* wmsg =  pbc_wmessage_new(ppbc_env, type_name);
	if (wmsg == NULL)
	{
		return ATOM_UNDEFINED;
	}
	if (encode_message(env, wmsg, type_name, argv[1]) < 0)
	{
		return enif_make_tuple2(env, ATOM_ERROR, enif_make_string(env, pbc_error(ppbc_env), ERL_NIF_LATIN1));
	}
	struct pbc_slice slice;
	ERL_NIF_TERM result;
	pbc_wmessage_buffer(wmsg, &slice);
	ErlNifBinary bin;
	enif_alloc_binary(slice.len, &bin);
	strncpy(bin.data, slice.buffer, slice.len);
	result = enif_make_binary(env, &bin);
	pbc_wmessage_delete(wmsg);
	return result;
}

ERL_NIF_TERM nif_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct pbc_env* ppbc_env = get_pbc_env();
	if (ppbc_env == NULL)
	{
		return ATOM_ERROR;
	}
	char type_name[100];
	ErlNifBinary bin;
	ERL_NIF_TERM term;
	int decode_type;
	struct pbc_slice slice;
	enif_get_string(env, argv[0], type_name, sizeof(type_name), ERL_NIF_LATIN1);
	enif_inspect_binary(env, argv[1], &bin);
	enif_get_int(env, argv[2], &decode_type);
	if (decode_type != decode_type_record && decode_type != decode_type_map)
	{
		decode_type = decode_type_record;
	}
	slice.buffer = bin.data;
	slice.len = bin.size;
	if (decode_message(env, type_name, &slice, &term, decode_type) < 0)
	{
		return enif_make_tuple2(env, ATOM_ERROR, enif_make_string(env, pbc_error(ppbc_env), ERL_NIF_LATIN1));
	}
	return term;
}

ERL_NIF_TERM nif_get_default(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	char type_name[100];
	ERL_NIF_TERM term = ATOM_UNDEFINED;
	type_name[99] = '\0';
	int decode_type = decode_type_record;
	enif_get_string(env, argv[0], type_name, sizeof(type_name), ERL_NIF_LATIN1);
	enif_get_int(env, argv[1], &decode_type);
	if (decode_type == decode_type_record)
	{
		get_default_record(enif_priv_data(env), type_name, &term);
	}
	else if (decode_type == decode_type_map)
	{
		get_default_map(enif_priv_data(env), type_name, &term);
	}
 	return enif_make_copy(env, term);
}

ERL_NIF_TERM nif_all_default(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	int decode_type = decode_type_record;
	enif_get_int(env, argv[0], &decode_type);
	if (decode_type == decode_type_record)
	{
		return enif_make_copy(env, default_records);
	}
	else if (decode_type == decode_type_map)
	{
		return enif_make_copy(env, default_maps);
	}
	return ATOM_UNDEFINED;
}

void read_file(const char *filename, struct pbc_slice *slice) {
	FILE *f = fopen(filename, "rb");
	if (f == NULL) {
		slice->buffer = NULL;
		slice->len = 0;
		return;
	}
	fseek(f, 0, SEEK_END);
	slice->len = ftell(f);
	fseek(f, 0, SEEK_SET);
	slice->buffer = malloc(slice->len);
	if (fread(slice->buffer, 1, slice->len, f) == 0)
		exit(1);
	fclose(f);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
	ErlNifEnv *env_priv = enif_alloc_env();
	*priv_data = env_priv;
	ATOM_OK = enif_make_atom(env, "ok");
	ATOM_ERROR = enif_make_atom(env, "error");
	ATOM_TRUE = enif_make_atom(env, "true");
	ATOM_FALSE = enif_make_atom(env, "false");
	ATOM_UNDEFINED = enif_make_atom(env, "undefined");
	file = fopen("pbc_nif_log.txt", "w+");
    init_thread_local();
	return 0;
}

static int on_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}
static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
	return 0;
}
static void on_unload(ErlNifEnv* env, void* priv_data)
{
	return;
}

ERL_NIF_INIT(pbc_nif, nif_funcs, &on_load, &on_reload, &on_upgrade, &on_unload);
