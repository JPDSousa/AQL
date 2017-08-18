
-module(index).

-include("aql.hrl").

-define(INDEX_CRDT, antidote_crdt_gset).
-define(ITAG_CRDT, antidote_crdt_gmap).
-define(ITAG_KEY_CRDT, antidote_crdt_mvreg).
-define(INDEX_TOKEN, "#_").
-define(TAG_TOKEN, "#__").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([keys/2,
        name/1,
        put/1, put/2]).
-export([tag_name/2,
        tag_key/2, tag_subkey/1,
        tag/5,
        tag_read/4]).

keys(TName, TxId) ->
  BoundObject = crdt:create_bound_object(name(TName), ?INDEX_CRDT, ?METADATA_BUCKET),
  {ok, [Res]} = antidote:read_objects(BoundObject, TxId),
  lists:map(fun(Key) -> element:create_key(Key, TName) end, Res).

name(TName) ->
  TNameStr = utils:to_list(TName),
  NameStr = lists:concat([?INDEX_TOKEN, TNameStr]),
  list_to_atom(NameStr).

put({Key, _Map, TName}) ->
  BoundObject = crdt:create_bound_object(name(TName), ?INDEX_CRDT, ?METADATA_BUCKET),
  crdt:add_all(BoundObject, Key).

put(Key, TxId) ->
  antidote:update_objects(put(Key), TxId).

tag_name(TName, Column) ->
  {TName, Column}.

tag_key(TName, Column) ->
  Key = tag_name(TName, Column),
  crdt:create_bound_object(Key, ?ITAG_CRDT, ?METADATA_BUCKET).

tag_subkey(CName) ->
  ?MAP_KEY(CName, ?ITAG_KEY_CRDT).

tag(TName, Column, Value, ITag) ->
  BoundObject = tag_key(TName, Column),
  MapOp = crdt:assign_lww(ITag),
  crdt:single_map_update(BoundObject, Value, ?ITAG_KEY_CRDT, MapOp).

tag(TName, Column, Value, ITag, TxId) ->
  antidote:update_objects(tag(TName, Column, Value, ITag), TxId).

tag_read(TName, CName, Value, TxId) ->
  Key = tag_key(TName, CName),
  {ok, [Map]} = antidote:read_objects(Key, TxId),
  SubKey = tag_subkey(Value),
  proplists:get_value(SubKey, Map).

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

name_test() ->
  Expected = '#_Test',
  ?assertEqual(Expected, name("Test")),
  ?assertEqual(Expected, name('Test')).

put_test() ->
  BoundObject = crdt:create_bound_object(key, map, test),
  Expected = crdt:add_all({'#_test', ?INDEX_CRDT, ?METADATA_BUCKET}, key),
  ?assertEqual(Expected, put(BoundObject)).

tag_name_test() ->
  ?assertEqual({test,id}, tag_name(test, id)).

tag_test() ->
  BoundObject = crdt:create_bound_object({test,id}, ?ITAG_CRDT, ?METADATA_BUCKET),
  ExpectedOp = crdt:assign_lww(ipa:touch_cascade()),
  Expected = crdt:single_map_update(BoundObject, "Sam", ?ITAG_KEY_CRDT, ExpectedOp),
  ?assertEqual(Expected, tag(test, id, "Sam", ipa:touch_cascade())).

-endif.
