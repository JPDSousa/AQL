
-module(ipa_fk_SUITE).

-include_lib("aql.hrl").
-include_lib("parser.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SHADOW_AB, {[{'FkC','FkB'},{'FkB','FkA'}],?CRDT_INTEGER}).
-define(SHADOW_ABC, {[{'FkD','FkC'},{'FkC','FkB'},{'FkB','FkA'}], ?CRDT_INTEGER}).
-define(SHADOW_BC, {[{'FkD','FkC'},{'FkC','FkB'}], ?CRDT_INTEGER}).


-export([init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
          all/0]).

-export([indirect_foreign_keys/1,
          touch_cascade/1,
          insert_multilevel/1,
          delete_basic/1, delete_multilevel/1,
          create_table_fail/1,
          reference_deleted_fail/1]).

init_per_suite(Config) ->
  tutils:create_single_table("FkA"),
  tutils:create_fk_table("FkB", "FkA"),
  tutils:create_fk_table("FkC", "FkB"),
  tutils:create_fk_table("FkD", "FkC"),
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_Case, Config) ->
    {ok, []} = tutils:aql("INSERT INTO FkA VALUES (1)"),
    {ok, []} = tutils:aql("INSERT INTO FkB VALUES (1, 1)"),
    {ok, []} = tutils:aql("INSERT INTO FkB VALUES (2, 1)"),
    {ok, []} = tutils:aql("INSERT INTO FkC VALUES (1, 1)"),
    {ok, []} = tutils:aql("INSERT INTO FkC VALUES (2, 1)"),
    {ok, []} = tutils:aql("INSERT INTO FkD VALUES (1, 1)"),
    {ok, []} = tutils:aql("INSERT INTO FkD VALUES (2, 1)"),
    Config.

end_per_testcase(_, _) ->
  {ok, []} = tutils:aql("DELETE FROM FkA WHERE ID = 1"),
  ok.

all() ->
  [
    indirect_foreign_keys,
    touch_cascade,
    insert_multilevel,
    delete_basic, delete_multilevel,
    create_table_fail,
    reference_deleted_fail
  ].

indirect_foreign_keys(_Config) ->
  KeyC = element:create_key('1', 'FkC'),
  KeyD = element:create_key('1', 'FkD'),
  {ok, [ResC, ResD], _CT} = antidote:read_objects([KeyC, KeyD]),
  ?assertEqual(1, proplists:get_value(?SHADOW_AB, ResC)),
  ?assertEqual(1, proplists:get_value(?SHADOW_ABC, ResD)),
  ?assertEqual(1, proplists:get_value(?SHADOW_BC, ResD)).

create_table_fail(_Config) ->
  % cannot create table that points to a non-existant table
  ?assertThrow(_, tutils:create_fk_table("FkETest", "FkFTest")),
  % cannot create a table that points to a non-existant column
  ?assertThrow(_, tutils:create_fk_table("FkETest", "FkA", "ABC")),
  % cannot create a table that points to a non-primary key column
  ?assertThrow(_, tutils:create_fk_table("FkETest", "FkB", "FkA")).

touch_cascade(_Config) ->
  tutils:assertExists(index:tag_key('FkB', [{'FkB', 'FkA'}])),
  tutils:assertExists(index:tag_key('FkC', [{'FkC', 'FkB'}])),
  {TypelessCAB, _TypeCAB} = ?SHADOW_AB,
  tutils:assertExists(index:tag_key('FkC', TypelessCAB)),
  tutils:assertExists(index:tag_key('FkD', [{'FkD', 'FkC'}])),
  {TypelessDABC, _TypeDABC} = ?SHADOW_ABC,
  tutils:assertExists(index:tag_key('FkD', TypelessDABC)),
  {TypelessDBC, _TypeDBC} = ?SHADOW_BC,
  tutils:assertExists(index:tag_key('FkD', TypelessDBC)).

insert_multilevel(_Config) ->
  %bottom level insert
  tutils:assertState(true, "FkA", "1"),
  tutils:assertState(true, "FkB", "1"),
  tutils:assertState(true, "FkB", "2"),
  tutils:assertState(true, "FkC", "1"),
  tutils:assertState(true, "FkC", "2"),
  % middle level insert
  tutils:aql("INSERT INTO FkB VALUES (1, 1)"),
  tutils:assertState(true, "FkC", "1"),
  tutils:assertState(true, "FkC", "2").

delete_basic(_Config) ->
  {ok, []} = tutils:delete_by_key("FkA", "1"),
  tutils:assertState(false, "FkA", "1"),
  tutils:assertState(false, "FkB", "1").

delete_multilevel(_Config) ->
  {ok, []} = tutils:delete_by_key("FkA", "1"),
  tutils:assertState(false, "FkA", "1"),
  tutils:assertState(false, "FkB", "1"),
  tutils:assertState(false, "FkB", "2"),
  tutils:assertState(false, "FkC", "1"),
  tutils:assertState(false, "FkB", "2"),
  tutils:assertState(false, "FkD", "1").

reference_deleted_fail(_Config) ->
  {ok, []} = tutils:delete_by_key("FkA", "1"),
  ?assertThrow(_, tutils:aql("INSERT INTO FkB VALUES (2, 1)")),
  ?assertThrow(_, tutils:aql("INSERT INTO FkC VALUES (1, 1)")).
