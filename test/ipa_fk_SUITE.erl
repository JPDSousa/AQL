
-module(ipa_fk_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-export([init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
          all/0]).

-export([insert_basic/1, insert_multilevel/1,
          delete_basic/1, delete_multilevel/1]).

init_per_suite(Config) ->
  tutils:create_single_table("FkA"),
  tutils:create_fk_table("FkB", "FkA"),
  tutils:create_fk_table("FkC", "FkB"),
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_, _) ->
ok.

all() ->
  [insert_basic,
  insert_multilevel,
  delete_basic,
  delete_multilevel].

insert_basic(_Config) ->
  % insert test
  {ok, []} = tutils:aql("INSERT INTO FkA VALUES (1)"),
  tutils:assertState(ipa:new(), "FkA", "1"),
  % touch test
  io:fwrite("B: ~p~n", [tutils:aql("SELECT * FROM FkB WHERE ID = 1")]),
  {ok, []} = tutils:aql("INSERT INTO FkB VALUES (1, 1)"),
  io:fwrite("A: ~p~n", [tutils:aql("SELECT * FROM FkB WHERE ID = 1")]),
  tutils:assertState(ipa:new(), "FkB", "1"),
  tutils:assertState(ipa:touch(), "FkA", "1"),
  % touch cascade test
  {ok, []} = tutils:aql("INSERT INTO FkB VALUES (2, 1)"),
  tutils:assertState(ipa:touch(), "FkA", "1"),
  tutils:assertState(ipa:touch_cascade(), "FkB", "1").

insert_multilevel(_Config) ->
  %bottom level insert
  load_multilevel_data(),
  tutils:assertState(ipa:touch(), "FkA", "1"),
  tutils:assertState(ipa:touch(), "FkB", "1"),
  tutils:assertState(ipa:touch_cascade(), "FkB", "2"),
  tutils:assertState(ipa:touch_cascade(), "FkC", "1"),
  tutils:assertState(ipa:new(), "FkC", "2"),
  % middle level insert
  tutils:aql("INSERT INTO FkB VALUES (1, 1)"),
  tutils:assertState(ipa:touch_cascade(), "FkC", "1"),
  tutils:assertState(ipa:touch_cascade(), "FkC", "2").

delete_basic(_Config) ->
  {ok, []} = tutils:aql("INSERT INTO FkA VALUES (1)"),
  {ok, []} = tutils:aql("INSERT INTO FkB VALUES (1, 1)"),
  {ok, []} = tutils:delete_by_key("FkA", "1"),
  tutils:assertState(ipa:delete(), "FkA", "1"),
  tutils:assertState(ipa:delete_cascade(), "FkB", "1").

delete_multilevel(_Config) ->
  load_multilevel_data(),
  {ok, []} = tutils:delete_by_key("FkA", "1"),
  tutils:assertState(ipa:delete(), "FkA", "1"),
  tutils:assertState(ipa:delete_cascade(), "FkB", "1"),
  tutils:assertState(ipa:delete_cascade(), "FkB", "2"),
  tutils:assertState(ipa:delete_cascade(), "FkC", "1"),
  tutils:assertState(ipa:delete_cascade(), "FkB", "2").

load_multilevel_data() ->
  {ok, []} = tutils:aql("INSERT INTO FkA VALUES (1)"),
  {ok, []} = tutils:aql("INSERT INTO FkB VALUES (1, 1)"),
  {ok, []} = tutils:aql("INSERT INTO FkB VALUES (2, 1)"),
  {ok, []} = tutils:aql("INSERT INTO FkC VALUES (1, 1)"),
  {ok, []} = tutils:aql("INSERT INTO FkC VALUES (2, 1)").
