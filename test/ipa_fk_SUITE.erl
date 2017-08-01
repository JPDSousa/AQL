
-module(ipa_fk_SUITE).

-include_lib("aql.hrl").
-include_lib("parser.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
          all/0]).

-export([indirect_foreign_keys/1,
          insert_multilevel/1,
          delete_basic/1, delete_multilevel/1,
          create_table_fail/1]).

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
  [indirect_foreign_keys,
  insert_multilevel
  %delete_basic, delete_multilevel,
  %create_table_fail
  ].

indirect_foreign_keys(_Config) ->
  KeyC = element:create_key('1', 'FkC'),
  KeyD = element:create_key('1', 'FkD'),
  {ok, [ResC, ResD], _CT} = antidote:read_objects([KeyC, KeyD]),
  ShadowAB = {{[{'ID','FkA'},{'ID','FkB'}]}, ?CRDT_INTEGER},
  ShadowABC = {{[{'ID','FkA'},{'ID','FkB'},{'ID','FkC'}]}, ?CRDT_INTEGER},
  ShadowBC = {{[{'ID','FkB'},{'ID','FkC'}]}, ?CRDT_INTEGER},
  ?assertEqual(1, proplists:get_value(ShadowAB, ResC)),
  ?assertEqual(1, proplists:get_value(ShadowABC, ResD)),
  ?assertEqual(1, proplists:get_value(ShadowBC, ResD)).

create_table_fail(_Config) ->
  % cannot create table that points to a non-existant table
  ?assertThrow(_, tutils:create_fk_table("FkETest", "FkFTest")),
  % cannot create a table that points to a non-existant column
  ?assertThrow(_, tutils:create_fk_table("FkETest", "FkA", "ABC")),
  % canot create a table that points to a non-primary key column
  ?assertThrow(_, tutils:create_fk_table("FkETest", "FkB", "FkA")).

insert_multilevel(_Config) ->
  %bottom level insert
  tutils:assertState(ipa:touch(), "FkA", "1"),
  tutils:assertState(ipa:touch(), "FkB", "1"),
  %tutils:assertState(ipa:touch_cascade(), "FkB", "2"),
  %tutils:assertState(ipa:touch_cascade(), "FkC", "1"),
  tutils:assertState(ipa:new(), "FkC", "2"),
  % middle level insert
  tutils:aql("INSERT INTO FkB VALUES (1, 1)").
  %tutils:assertState(ipa:touch_cascade(), "FkC", "1"),
  %tutils:assertState(ipa:touch_cascade(), "FkC", "2").

delete_basic(_Config) ->
  {ok, []} = tutils:aql("INSERT INTO FkA VALUES (1)"),
  {ok, []} = tutils:aql("INSERT INTO FkB VALUES (1, 1)"),
  {ok, []} = tutils:delete_by_key("FkA", "1"),
  tutils:assertState(ipa:delete(), "FkA", "1"),
  tutils:assertState(ipa:delete_cascade(), "FkB", "1").

delete_multilevel(_Config) ->
  {ok, []} = tutils:delete_by_key("FkA", "1"),
  tutils:assertState(ipa:delete(), "FkA", "1"),
  tutils:assertState(ipa:delete_cascade(), "FkB", "1"),
  tutils:assertState(ipa:delete_cascade(), "FkB", "2"),
  tutils:assertState(ipa:delete_cascade(), "FkC", "1"),
  tutils:assertState(ipa:delete_cascade(), "FkB", "2").
