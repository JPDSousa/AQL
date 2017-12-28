
-module(bcounter_SUITE).

-include_lib("parser.hrl").
-include_lib("aql.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ct_aql.hrl").

-define(UPDATE_ERROR, {error, "Unexpected error"}).
-define(INSERT_ERROR(Col), {error, "Invalid value 0 for column " ++ Col}).

-export([init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
          all/0]).

-export([greater_insert_basic/1, greater_insert_fail/1,
        greater_update_basic/1, greater_update_fail/1,
        smaller_insert_basic/1, smaller_insert_fail/1,
        smaller_update_basic/1, smaller_update_fail/1]).

%% ====================================================================
%% CT config functions
%% ====================================================================

init_per_suite(Config) ->
  TNameGreater = "BCGreater",
  TNameSmaller = "BCSmaller",
  BoundGreaterA = 0,
  BoundGreaterB = 10,
  BoundSmallerA = 5,
  BoundSmallerB = 15,
  Query = ["CREATE @AW TABLE ", TNameGreater, " (ID INT PRIMARY KEY, ",
  "bcA COUNTER_INT CHECK GREATER ", BoundGreaterA, ", ",
  "bcB COUNTER_INT CHECK GREATER ", BoundGreaterB,
  ");",
  "CREATE @AW TABLE ", TNameSmaller, " (ID INT PRIMARY KEY, ",
  "bcA COUNTER_INT CHECK SMALLER ", BoundSmallerA, ", ",
  "bcB COUNTER_INT CHECK SMALLER ", BoundSmallerB,
  ");"],
  {ok, []} = tutils:aql(lists:concat(Query)),
  lists:append(Config, [
    {tname_greater, TNameGreater},
    {tname_smaller, TNameSmaller},
    {bound_greater_a, BoundGreaterA},
    {bound_greater_b, BoundGreaterB},
    {bound_smaller_a, BoundSmallerA},
    {bound_smaller_b, BoundSmallerB},
    {insert_greater, lists:concat(["INSERT INTO ", TNameGreater, " VALUES (~p, ~p, ~p)"])},
    {insert_smaller, lists:concat(["INSERT INTO ", TNameSmaller, " VALUES (~p, ~p, ~p)"])},
    {update_greater, lists:concat(["UPDATE ", TNameGreater, " SET bcA ~s AND bcB ~s WHERE ID = ~p"])},
    {update_smaller, lists:concat(["UPDATE ", TNameSmaller, " SET bcA ~s AND bcB ~s WHERE ID = ~p"])}
  ]).

end_per_suite(Config) ->
  Config.

init_per_testcase(_Case, Config) ->
  Config.

end_per_testcase(_, _) ->
  ok.

all() ->
  [greater_insert_basic,  greater_insert_fail,
  greater_update_basic, greater_update_fail,
  smaller_insert_basic, smaller_insert_fail,
  smaller_update_basic, smaller_update_fail].

%% ====================================================================
%% Test functions
%% ====================================================================

greater_insert_basic(Config) ->
  Key = 1,
  BoundA = ?value(bound_greater_a, Config),
  BoundB = ?value(bound_greater_b, Config),
  BcA = BoundA+2,
  BcB = BoundB+1,
  {ok, []} = tutils:aql(?format(insert_greater, [Key, BcA, BcB], Config)),
  [V1, V2] = tutils:read_keys(?value(tname_greater, Config), Key, ["bcA, bcB"]),
  ?assertEqual(BcA, V1),
  ?assertEqual(BcB, V2),
  reset_counters(Key, ?GREATER_TOKEN, BcA, BcB, Config),
  [V3, V4] = tutils:read_keys(?value(tname_greater, Config), Key, ["bcA, bcB"]),
  ?assertEqual(BoundA, V3),
  ?assertEqual(BoundB, V4).

greater_insert_fail(Config) ->
  BoundA = ?value(bound_greater_a, Config),
  BoundB = ?value(bound_greater_b, Config),
  ?assertEqual(?INSERT_ERROR("bcA"), tutils:aql(?format(insert_greater, [10, BoundA, BoundB+1], Config))),
  ?assertEqual(?INSERT_ERROR("bcB"), tutils:aql(?format(insert_greater, [11, BoundA+1, BoundB], Config))),
  ?assertEqual(?INSERT_ERROR("bcA"), tutils:aql(?format(insert_greater, [12, BoundA, BoundB-1], Config))).

greater_update_basic(Config) ->
  TName = ?value(tname_greater, Config),
  BoundA = ?value(bound_greater_a, Config),
  BoundB = ?value(bound_greater_b, Config),
  Key = 20,
  % inserts
  {ok, []} = tutils:aql(?format(insert_greater, [Key, BoundA+2, BoundB+2], Config)),
  % increment
  {ok, []} = tutils:aql(?format(update_greater, ["INCREMENT 1", "INCREMENT 3", Key], Config)),
  {ok, []} = tutils:aql(?format(update_greater, ["INCREMENT 0", "INCREMENT 0", Key], Config)),
  [V1, V2] = tutils:read_keys(TName, Key, ["bcA", "bcB"]),
  ?assertEqual(BoundA+3, V1),
  ?assertEqual(BoundB+5, V2),
  % decrement
  {ok, []} = tutils:aql(?format(update_greater, ["DECREMENT 2", "DECREMENT 4", Key], Config)),
  {ok, []} = tutils:aql(?format(update_greater, ["DECREMENT 0", "DECREMENT 0", Key], Config)),
  [V3, V4] = tutils:read_keys(TName, Key, ["bcA", "bcB"]),
  ?assertEqual(BoundA+1, V3),
  ?assertEqual(BoundB+1, V4),
  reset_counters(Key, ?GREATER_TOKEN, BoundA+1, BoundB+1, Config).

greater_update_fail(Config) ->
  TName = ?value(tname_greater, Config),
  BoundA = ?value(bound_greater_a, Config),
  BoundB = ?value(bound_greater_b, Config),
  Key = 30,
  % inserts
  {ok, []} = tutils:aql(?format(insert_greater, [Key, BoundA+2, BoundB+2], Config)),
  % decrement 1
  {ok, [Decrement1]} = tutils:aql(?format(update_greater, ["DECREMENT 3", "DECREMENT 3", Key], Config)),
  ?assertEqual(?UPDATE_ERROR, Decrement1),
  [V1, V2] = tutils:read_keys(TName, Key, ["bcA", "bcB"]),
  ?assertEqual(BoundA+2, V1), % assert value does not change on fail
  ?assertEqual(BoundB+2, V2),
  % decrement 2
  {ok, [Decrement2]} = tutils:aql(?format(update_greater, ["DECREMENT 4", "DECREMENT 4", Key], Config)),
  ?assertEqual(?UPDATE_ERROR, Decrement2),
  [V1, V2] = tutils:read_keys(TName, Key, ["bcA", "bcB"]),
  ?assertEqual(BoundA+2, V1), % assert value does not change on fail
  ?assertEqual(BoundB+2, V2),
  reset_counters(Key, ?GREATER_TOKEN, BoundA+2, BoundB+2, Config).

smaller_insert_basic(Config) ->
  TName = ?value(tname_smaller, Config),
  Key = 1,
  BoundA = ?value(bound_smaller_a, Config),
  BoundB = ?value(bound_smaller_b, Config),
  {ok, []} = tutils:aql(?format(insert_smaller, [Key, BoundA-1, BoundB-1], Config)),
  [V1, V2] = tutils:read_keys(TName, Key, ["bcA", "bcB"]),
  ?assertEqual(BoundA-1, V1),
  ?assertEqual(BoundB-1, V2),
  reset_counters(Key, ?SMALLER_TOKEN, BoundA-1, BoundB-1, Config).

smaller_insert_fail(Config) ->
  BoundA = ?value(bound_smaller_a, Config),
  BoundB = ?value(bound_smaller_b, Config),
  ?assertEqual(?INSERT_ERROR("bcA"), tutils:aql(?format(insert_smaller, [10, BoundA, BoundB-1], Config))),
  ?assertEqual(?INSERT_ERROR("bcB"), tutils:aql(?format(insert_smaller, [11, BoundA-1, BoundB], Config))),
  ?assertEqual(?INSERT_ERROR("bcA"), tutils:aql(?format(insert_smaller, [12, BoundA, BoundB], Config))).

smaller_update_basic(Config) ->
  TName = ?value(tname_smaller, Config),
  BoundA = ?value(bound_smaller_a, Config),
  BoundB = ?value(bound_smaller_b, Config),
  Key = 20,
  % inserts
  {ok, []} = tutils:aql(?format(insert_smaller, [Key, BoundA-3, BoundB-3], Config)),
  % increment
  {ok, []} = tutils:aql(?format(update_smaller, ["INCREMENT 2", "INCREMENT 2", Key], Config)),
  [V1, V2] = tutils:read_keys(TName, Key, ["bcA", "bcB"]),
  ?assertEqual(BoundA-1, V1),
  ?assertEqual(BoundB-1, V2),
  % decrement
  {ok, []} = tutils:aql(?format(update_smaller, ["DECREMENT 2", "DECREMENT 4", Key], Config)),
  [V3, V4] = tutils:read_keys(TName, Key, ["bcA", "bcB"]),
  ?assertEqual(BoundA-3, V3),
  ?assertEqual(BoundB-5, V4),
  reset_counters(Key, ?SMALLER_TOKEN, BoundA-3, BoundB-5, Config).

smaller_update_fail(Config) ->
  TName = ?value(tname_smaller, Config),
  BoundA = ?value(bound_smaller_a, Config),
  BoundB = ?value(bound_smaller_b, Config),
  Key = 30,
  % inserts
  {ok, []} = tutils:aql(?format(insert_smaller, [Key, BoundA-2, BoundB-2], Config)),
  % decrement
  {ok, [Decrement1]} = tutils:aql(?format(update_smaller, ["INCREMENT 3", "INCREMENT 3", Key], Config)),
  ?assertEqual(?UPDATE_ERROR, Decrement1),
  [V1, V2] = tutils:read_keys(TName, Key, ["bcA", "bcB"]),
  ?assertEqual(BoundA-2, V1), % assert value does not change on fail
  ?assertEqual(BoundB-2, V2),
  {ok, [Decrement2]} = tutils:aql(?format(update_smaller, ["INCREMENT 3", "INCREMENT 3", Key], Config)),
  ?assertEqual(?UPDATE_ERROR, Decrement2),
  [V3, V4] = tutils:read_keys(TName, Key, ["bcA", "bcB"]),
  ?assertEqual(BoundA-2, V3), % assert value does not change on fail
  ?assertEqual(BoundB-2, V4),
  reset_counters(Key, ?SMALLER_TOKEN, BoundA-2, BoundB-2, Config).

%% ====================================================================
%% Utils functions
%% ====================================================================

reset_counters(Key, Comp, BcA, BcB, Config) ->
  {InvBcA, InvBcB} = invert(Comp, BcA, BcB, Config),
  Updates = gen_reset_updates(Key, Comp, InvBcA, InvBcB),
  Query = ?format(update_key(Comp), Updates, Config),
  ct:log(info, lists:concat(["Reseting countets: ", Query])),
  {ok, []} = tutils:aql(Query).

update_key(?GREATER_TOKEN) -> update_greater;
update_key(?SMALLER_TOKEN) -> update_smaller.

gen_reset_updates(Key, ?GREATER_TOKEN, BcA, BcB) ->
  gen_reset_updates(Key, "DECREMENT", BcA, BcB);
gen_reset_updates(Key, ?SMALLER_TOKEN, BcA, BcB) ->
  gen_reset_updates(Key, "INCREMENT", BcA, BcB);
gen_reset_updates(Key, Op, BcA, BcB) ->
  [
    lists:concat([Op, " ", BcA]),
    lists:concat([Op, " ", BcB]),
    Key
  ].

invert(Comp, BcA, BcB, Config) ->
  {OffA, OffB} = offset(Comp, Config),
  InvBcA = bcounter:to_bcounter(none, BcA, OffA, Comp),
  InvBcB = bcounter:to_bcounter(none, BcB, OffB, Comp),
  {InvBcA, InvBcB}.

offset(?GREATER_TOKEN, Config) ->
  OffA = ?value(bound_greater_a, Config),
  OffB = ?value(bound_greater_b, Config),
  {OffA, OffB};
offset(?SMALLER_TOKEN, Config) ->
  OffA = ?value(bound_smaller_a, Config),
  OffB = ?value(bound_smaller_b, Config),
  {OffA, OffB}.
