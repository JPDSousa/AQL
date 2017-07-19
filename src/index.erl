
-module(index).

-include("aql.hrl").

-define(CRDT_TYPE, antidote_crdt_gset).
-define(INDEX_TOKEN, "#_").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([name/1,
        put/2]).

name(TName) when is_atom(TName) ->
  name(atom_to_list(TName));
name(TName) when is_list(TName) ->
  NameStr = lists:concat([?INDEX_TOKEN, TName]),
  list_to_atom(NameStr).

put({Key, _Map, TName}) ->
  BoundObject = crdt:create_bound_object(name(TName), ?CRDT_TYPE, ?METADATA_BUCKET),
  crdt:add_all(BoundObject, Key).

put(Key, TxId) ->
  antidote:update_objects(put(Key), TxId).


%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

name_test() ->
  Expected = '#_Test',
  ?assertEqual(Expected, name("Test")),
  ?assertEqual(Expected, name('Test')).

put_test() ->
  BoundObject = {key, map, test},
  Expected = crdt:add_all({'#_test', ?CRDT_TYPE, ?METADATA_BUCKET}, key),
  ?assertEqual(Expected, put(BoundObject)).

-endif.
