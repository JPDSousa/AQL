%%%-------------------------------------------------------------------
%%% @author joao
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. ago 2017 14:18
%%%-------------------------------------------------------------------
-module(aql_SUITE).
-author("joao").

-include_lib("aql.hrl").
-include_lib("parser.hrl").
-include_lib("types.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2,
  end_per_testcase/2,
  all/0]).


%% API
-export([select_all/1]).

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_, _) ->
  ok.

all() ->
  [
    select_all
  ].

select_all(_Config) ->
  TName = "SelectAll",
  {ok, []} = tutils:aql(lists:concat(["CREATE @AW TABLE ", TName, " (ID INTEGER PRIMARY KEY, Test VARCHAR)"])),
  {ok, []} = tutils:select_all("SelectAll"),
  {ok, []} = tutils:aql(lists:concat([
    "INSERT INTO ", TName, " VALUES (1, 'a');",
    "INSERT INTO ", TName, " VALUES (2, 'a');",
    "INSERT INTO ", TName, " VALUES (3, 'a');"
  ])),
  FullRes = tutils:select_all("SelectAll"),
  io:fwrite("FullRes: ~p~n", [FullRes]).
