#ifndef PBC_INNER_H
#define PBC_INNER_H
#include "pbc.h"
#include "erl_nif.h"

#define decode_type_record 0
#define decode_type_map 1

#ifdef __cplusplus
extern "C" {
#endif
int encode_message(ErlNifEnv* env, void *wmsg, const char *type_name, ERL_NIF_TERM value);
int decode_message(ErlNifEnv* env, const char *type_name, struct pbc_slice* slice, ERL_NIF_TERM* result, int decode_type);

int init_default(ErlNifEnv* env);
int get_default_record(ErlNifEnv* env, const char* type_name, ERL_NIF_TERM* term);
int get_default_map(ErlNifEnv* env, const char* type_name, ERL_NIF_TERM* default_map);

int init_thread_local();
void set_pbc_env(struct pbc_env* ppbc_env);
struct pbc_env* get_pbc_env();

#ifdef __cplusplus
}
#endif

#endif
