#ifndef PBC_ERL_H
#define PBC_ERL_H
#include <stdlib.h>
#include <string.h>

int pbc_erl_pbc_type_ip(struct _message *m, int id, const char** key, const char** type);
int pbc_erl_pbc_type_sp(struct _message *m, const char *key, const char ** type);

int pbc_erl_field_count(struct _message *m);

#endif // !PBC_ERL_H