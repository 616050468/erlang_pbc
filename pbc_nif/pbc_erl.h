#ifndef PBC_ERL_H
#define PBC_ERL_H
#include <stdlib.h>
#include <string.h>
struct _message;
int pbc_erl_pbc_type_index(struct _message *m, int index, const char** key, const char** type);
int pbc_erl_pbc_type_sp(struct _message *m, const char *key, const char ** type);

int pbc_erl_field_count(struct _message *m);

#endif // !PBC_ERL_H
