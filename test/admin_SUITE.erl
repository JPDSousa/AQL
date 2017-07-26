
-module(admin_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
          all/0]).

-export([show_tables/1,
        show_index/1]).

%% ====================================================================
%% CT config functions
%% ====================================================================

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_, _) ->
  ok.

all() ->
  [show_tables, show_index].

%% ====================================================================
%% Test functions
%% ====================================================================

show_tables(_Config) ->
  tutils:create_single_table("ShowTablesTest"),
  {ok, [Res]} = tutils:aql("SHOW TABLES"),
  ?assertEqual(true, is_list(Res)).

show_index(_Config) ->
  tutils:create_single_table("ShowIndexTest"),
  {ok, []} = tutils:aql("INSERT INTO ShowIndexTest VALUES (1)"),
  {ok, []} = tutils:aql("INSERT INTO ShowIndexTest VALUES (2)"),
  {ok, []} = tutils:aql("INSERT INTO ShowIndexTest VALUES (3)"),
  {ok, [Index]} = tutils:aql("SHOW INDEX FROM ShowIndexTest"),
  ?assertEqual(3, length(Index)).
