#include "pbc_nif.h"

#if defined( __WIN32__ ) || defined( WIN32 ) || defined( _WIN32 )
#include <windows.h>
static DWORD dwTlsIndex;
int init_thread_local()
{
    if((dwTlsIndex = TlsAlloc()) == TLS_OUT_OF_INDEXES)
    {
        return 0;
    }
    return 1;
}

void set_pbc_env(struct pbc_env* ppbc_env)
{
    TlsSetValue(dwTlsIndex, ppbc_env);
}

struct pbc_env* get_pbc_env()
{
    TlsGetValue(dwTlsIndex);
}

#else
#include <pthread.h>
static pthread_key_t thread_pbc_env_key;

int init_thread_local()
{
    pthread_key_create(&thread_pbc_env_key, NULL);
    return 1;
}
void set_pbc_env(struct pbc_env* ppbc_env)
{
    pthread_setspecific(thread_pbc_env_key, ppbc_env);
    return;
}
struct pbc_env* get_pbc_env()
{
    return (struct pbc_env*)pthread_getspecific(thread_pbc_env_key);
}
#endif




