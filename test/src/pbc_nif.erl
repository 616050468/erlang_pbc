-module(pbc_nif).
-on_load(init/0).
-compile(export_all).

init() ->
    erlang:load_nif("priv/pbc_nif", 0).

register(File) ->
    erlang:nif_error({error, not_loaded}).

%% Term: record or map
encode(MsgType, Term) ->
    erlang:nif_error({error, not_loaded}).

%% Type 0: record, 1: map
decode(MsgType, Bin, Type) ->
    erlang:nif_error({error, not_loaded}).

get_default(MsgType) ->
    erlang:nif_error({error, not_loaded}).