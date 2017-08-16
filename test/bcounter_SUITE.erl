
-module(bcounter_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
          all/0]).

-export([greater_insert_basic/1, greater_insert_fail/1,
        greater_udpate_basic/1, greater_update_fail/1,
        smaller_insert_basic/1, smaller_insert_fail/1,
        smaller_update_basic/1, smaller_update_fail/1]).

%% ====================================================================
%% CT config functions
%% ====================================================================

init_per_suite(Config) ->
  Query = ["CREATE @AW TABLE BCGreater (ID INT PRIMARY KEY, ",
  "bcA COUNTER_INT CHECK GREATER 0, ",
  "bcB COUNTER_INT CHECK GREATER 10",
  ");",
  "CREATE @AW TABLE BCSmaller (ID INT PRIMARY KEY, ",
  "bcA COUNTER_INT CHECK SMALLER 5, ",
  "bcB COUNTER_INT CHECK SMALLER 15",
  ");"],
  {ok, []} = tutils:aql(lists:concat(Query)),
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_, _) ->
  ok.

all() ->
  [greater_insert_basic,  greater_insert_fail,
  greater_udpate_basic, greater_update_fail,
  smaller_insert_basic, smaller_insert_fail,
  smaller_update_basic, smaller_update_fail].

%% ====================================================================
%% Test functions
%% ====================================================================

greater_insert_basic(_Config) ->
  {ok, []} = tutils:aql("INSERT INTO BCGreater VALUES (1, 2, 11)"),
  [V1, V2] = tutils:read_keys("BCGreater", "1", ["bcA, bcB"]),
  ?assertEqual(2, V1),
  ?assertEqual(11, V2).

greater_insert_fail(_Config) ->
  ?assertThrow(_, tutils:aql("INSERT INTO BCGreater VALUES (10, 0, 11)")),
  ?assertThrow(_, tutils:aql("INSERT INTO BCGreater VALUES (11, 1, 10)")),
  ?assertThrow(_, tutils:aql("INSERT INTO BCGreater VALUES (12, 1, 5)")).

greater_udpate_basic(_Config) ->
  % inserts
  {ok, []} = tutils:aql("INSERT INTO BCGreater VALUES (20, 2, 12)"),
  % increment
  {ok, []} = tutils:aql("UPDATE BCGreater SET bcA INCREMENT 1 WHERE ID = 20"),
  {ok, []} = tutils:aql("UPDATE BCGreater SET bcB INCREMENT 3 WHERE ID = 20"),
  [V1, V2] = tutils:read_keys("BCGreater", "20", ["bcA", "bcB"]),
  ?assertEqual(3, V1),
  ?assertEqual(15, V2),
  % decrement
  {ok, []} = tutils:aql("UPDATE BCGreater SET bcA DECREMENT 2 WHERE ID = 20"),
  {ok, []} = tutils:aql("UPDATE BCGreater SET bcB DECREMENT 4 WHERE ID = 20"),
  [V3, V4] = tutils:read_keys("BCGreater", "20", ["bcA", "bcB"]),
  ?assertEqual(1, V3),
  ?assertEqual(11, V4).

greater_update_fail(_Config) ->
  % inserts
  {ok, []} = tutils:aql("INSERT INTO BCGreater VALUES (30, 2, 12)"),
  % decrement
  ?assertThrow(_, tutils:aql("UPDATE BCGreater SET bcA DECREMENT 3 WHERE ID = 30")),
  ?assertThrow(_, tutils:aql("UPDATE BCGreater SET bcB DECREMENT 3 WHERE ID = 30")),
  [V1, V2] = tutils:read_keys("BCGreater", "30", ["bcA", "bcB"]),
  ?assertEqual(2, V1), % assert value does not change on fail
  ?assertEqual(12, V2),
  ?assertThrow(_, tutils:aql("UPDATE BCGreater SET bcA DECREMENT 4 WHERE ID = 30")),
  ?assertThrow(_, tutils:aql("UPDATE BCGreater SET bcB DECREMENT 4 WHERE ID = 30")),
  [V1, V2] = tutils:read_keys("BCGreater", "30", ["bcA", "bcB"]),
  ?assertEqual(2, V1), % assert value does not change on fail
  ?assertEqual(12, V2).

smaller_insert_basic(_Config) ->
  {ok, []} = tutils:aql("INSERT INTO BCSmaller VALUES (1, 4, 14)"),
  [V1, V2] = tutils:read_keys("BCSmaller", "1", ["bcA", "bcB"]),
  ?assertEqual(4, V1),
  ?assertEqual(14, V2).

smaller_insert_fail(_Config) ->
  ?assertThrow(_, tutils:aql("INSERT INTO BCSmaller VALUES (10, 5, 14)")),
  ?assertThrow(_, tutils:aql("INSERT INTO BCSmaller VALUES (11, 4, 15)")),
  ?assertThrow(_, tutils:aql("INSERT INTO BCSmaller VALUES (12, 5, 15)")).

smaller_update_basic(_Config) ->
  % inserts
  {ok, []} = tutils:aql("INSERT INTO BCSmaller VALUES (20, 2, 12)"),
  % increment
  {ok, []} = tutils:aql("UPDATE BCSmaller SET bcA INCREMENT 2 WHERE ID = 20"),
  {ok, []} = tutils:aql("UPDATE BCSmaller SET bcB INCREMENT 2 WHERE ID = 20"),
  [V1, V2] = tutils:read_keys("BCSmaller", "20", ["bcA", "bcB"]),
  ?assertEqual(4, V1),
  ?assertEqual(14, V2),
  % decrement
  {ok, []} = tutils:aql("UPDATE BCSmaller SET bcA DECREMENT 2 WHERE ID = 20"),
  {ok, []} = tutils:aql("UPDATE BCSmaller SET bcB DECREMENT 4 WHERE ID = 20"),
  [V3, V4] = tutils:read_keys("BCSmaller", "20", ["bcA", "bcB"]),
  ?assertEqual(2, V3),
  ?assertEqual(10, V4).

smaller_update_fail(_Config) ->
  % inserts
  {ok, []} = tutils:aql("INSERT INTO BCSmaller VALUES (30, 3, 13)"),
  % decrement
  ?assertThrow(_, tutils:aql("UPDATE BCSmaller SET bcA INCREMENT 3 WHERE ID = 30")),
  ?assertThrow(_, tutils:aql("UPDATE BCSmaller SET bcB INCREMENT 3 WHERE ID = 30")),
  [V1, V2] = tutils:read_keys("BCSmaller", "30", ["bcA", "bcB"]),
  ?assertEqual(3, V1), % assert value does not change on fail
  ?assertEqual(13, V2),
  ?assertThrow(_, tutils:aql("UPDATE BCSmaller SET bcA INCREMENT 3 WHERE ID = 30")),
  ?assertThrow(_, tutils:aql("UPDATE BCSmaller SET bcB INCREMENT 3 WHERE ID = 30")),
  [V3, V4] = tutils:read_keys("BCSmaller", "30", ["bcA", "bcB"]),
  ?assertEqual(3, V3), % assert value does not change on fail
  ?assertEqual(13, V4).
