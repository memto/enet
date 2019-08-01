#define _WINSOCK_DEPRECATED_NO_WARNINGS
#define ENET_IMPLEMENTATION
#include "enet.h"

#include "erl_nif.h"
#include "erl_driver.h"

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, int errnum);
void enet_alloc_free(ErlNifEnv *env, void *obj);

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_eagain;
static ERL_NIF_TERM atom_undefined;

static ErlNifResourceType *ENET_ALLOC_RESOURCE;

typedef struct _procket_alloc_state {
    size_t size;
    void *buf;
} ENET_ALLOC_STATE;

#define ENET_REALLOC(bin, nsize) do { \
    size_t osize = bin.size; \
    if (nsize != bin.size) { \
        if (!enif_realloc_binary(&bin, nsize)) \
            return error_tuple(env, ENOMEM); \
        if (nsize > osize) \
            (void)memset(bin.data+osize, 0, bin.size-osize); \
    } \
} while (0);

load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_eagain = enif_make_atom(env, "eagain");
    atom_undefined = enif_make_atom(env, "undefined");

    if ( (ENET_ALLOC_RESOURCE = enif_open_resource_type(env, NULL,
        "enet_alloc_resource", enet_alloc_free,
        ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    return (0);
}

/* Stubs for reload and upgrade */
static int
reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM info)
{
  return 0;
}

ENetRangeCoder context;

/* 0: inData */
static ERL_NIF_TERM
nif_enet_decompress(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int fd = -1;
    unsigned long inLimit = 0;
    ErlNifBinary inData = {0};

    ErlNifBinary outData = {0};

    if (!enif_inspect_binary(env, argv[0], &inData))
        return enif_make_badarg(env);

    if (!enif_alloc_binary(ENET_PROTOCOL_MAXIMUM_MTU, &outData))
        return error_tuple(env, ENOMEM);

    size_t outLen = enet_range_coder_decompress(&context, inData.data, inData.size, outData.data, outData.size);

    ENET_REALLOC(outData, outLen);

    return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &outData));
}

void
enet_alloc_free(ErlNifEnv *env, void *obj)
{
    ENET_ALLOC_STATE *p = obj;

    if (p->buf == NULL)
        return;

    free(p->buf);
    p->buf = NULL;
    p->size = 0;
}

static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, int errnum)
{
    return enif_make_tuple2(env, atom_error,
            enif_make_atom(env, erl_errno_id(errnum)));
}

static ErlNifFunc nif_funcs[] = {
    {"decompress", 1, nif_enet_decompress},
};

ERL_NIF_INIT(enet_compress, nif_funcs, load, reload, upgrade, NULL)
