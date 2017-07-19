
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

-export([name/1,
        put/1, put/2,
        name_tag/2,
        tag/4, tag/5]).

name(TName) ->
  TNameStr = utils:to_list(TName),
  NameStr = lists:concat([?INDEX_TOKEN, TNameStr]),
  list_to_atom(NameStr).

put({Key, _Map, TName}) ->
  BoundObject = crdt:create_bound_object(name(TName), ?INDEX_CRDT, ?METADATA_BUCKET),
  crdt:add_all(BoundObject, Key).

put(Key, TxId) ->
  antidote:update_objects(put(Key), TxId).

name_tag(TName, Column) ->
  TNameStr = utils:to_list(TName),
  ColumnStr = utils:to_list(Column),
  NameStr = lists:concat([?TAG_TOKEN, TNameStr, "_", ColumnStr]),
  list_to_atom(NameStr).

tag(TName, Column, Value, ITag) ->
  Name = name_tag(TName, Column),
  BoundObject = crdt:create_bound_object(Name, ?ITAG_CRDT, ?METADATA_BUCKET),
  MapOp = crdt:assign_lww(ITag),
  crdt:single_map_update(BoundObject, Value, ?ITAG_KEY_CRDT, MapOp).

tag(TName, Column, Value, ITag, TxId) ->
  antidote:update_objects(tag(TName, Column, Value, ITag), TxId).

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

name_tag_test() ->
  Expected = '#__test_id',
  ?assertEqual(Expected, name_tag("test", "id")),
  ?assertEqual(Expected, name_tag(test, "id")),
  ?assertEqual(Expected, name_tag("test", id)),
  ?assertEqual(Expected, name_tag(test, id)).

tag_test() ->
  BoundObject = crdt:create_bound_object('#__test_id', ?ITAG_CRDT, ?METADATA_BUCKET),
  ExpectedOp = crdt:assign_lww(ipa:touch_cascade()),
  Expected = crdt:single_map_update(BoundObject, "Sam", ?ITAG_KEY_CRDT, ExpectedOp),
  ?assertEqual(Expected, tag("test", "id", "Sam", ipa:touch_cascade())).

-endif.
