#include "proto.h"
#include "pbc_erl.h"
int
pbc_erl_pbc_type_ip(struct _message *m, int id, const char **key, const char ** type) {
	if (m == NULL) {
		return 0;
	}
	struct _field * field = (struct _field *)_pbcM_ip_query(m->id, id);
	if (field == NULL)
	{
		return PBC_NOEXIST;
	}
	if (key)
		*key = field->name;
	return _pbcP_type(field, type);
}

int
pbc_erl_pbc_type_sp(struct _message *m, const char *key, const char ** type) {
	if (m == NULL) {
		return 0;
	}
	if (key == NULL)
	{
		return PBC_NOEXIST;
	}
	struct _field * field = (struct _field *)_pbcM_sp_query(m->name, key);
	return _pbcP_type(field, type);
}

void field_count_cb(void *p, void *ud)
{
	int *i = (int*)ud;
	*i = 1 + (*i);
}

int pbc_erl_field_count(struct _message *m)
{
	int i = 0;
	_pbcM_sp_foreach_ud(m->name, field_count_cb, &i);
	return i;
}
