%% @author JPDSousa
%% @doc bounded counter converter

-module(bcounter).

-include_lib("parser.hrl").
-include_lib("aql.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type bcounter() :: antidote_crdt_bcounter:bcounter().

-export([to_bcounter/4,
        from_bcounter/3,
        value/1]).

-spec to_bcounter(key(), integer(), integer(), comparator()) -> integer().
to_bcounter(Key, Value, Offset, Comp) ->
  OffValue = apply_offset_value(Comp, Offset, Value),
  check_bcounter_value(Key, OffValue).

check_bcounter_value(_Key, Value) when Value > 0 -> Value;
check_bcounter_value(Key, Value) -> throw(lists:concat(["Invalid value ", Value, " for column ", Key])).

apply_offset_value(?GREATER_TOKEN, Offset, Value) -> Value-Offset;
apply_offset_value(?SMALLER_TOKEN, Offset, Value) -> Offset-Value.

-spec from_bcounter(comparator(), bcounter(), integer()) -> integer().
from_bcounter(Comp, {_I, _D} = Value, Offset) ->
  BCValue = value(Value),
  from_bcounter(Comp, BCValue, Offset);
from_bcounter(?GREATER_TOKEN, Value, Offset) ->
  Value+Offset;
from_bcounter(?SMALLER_TOKEN, Value, Offset) -> Offset-Value.

-spec value(bcounter()) -> integer().
value({Incs, Decs}) ->
  IncsList = orddict:to_list(Incs),
  DecsList = orddict:to_list(Decs),
  SumIncs = lists:foldl(fun sum/2, 0, IncsList),
  SumDecs = lists:foldl(fun sum/2, 0, DecsList),
  SumIncs-SumDecs.

sum({_Ids, Value}, Acc) -> Value+Acc.

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

to_bcounter_test() ->
  % greater than
  ?assertEqual(5, to_bcounter(k, 5, 0, ?GREATER_TOKEN)),
  ?assertEqual(3, to_bcounter(k, 5, 2, ?GREATER_TOKEN)),
  ?assertEqual(1, to_bcounter(k, 31, 30, ?GREATER_TOKEN)),
  ?assertThrow(_, to_bcounter(k, 5, 6, ?GREATER_TOKEN)),
  ?assertThrow(_, to_bcounter(k, 5, 5, ?GREATER_TOKEN)),
  % smaller than
  ?assertEqual(5, to_bcounter(k, 0, 5, ?SMALLER_TOKEN)),
  ?assertEqual(3, to_bcounter(k, 2, 5, ?SMALLER_TOKEN)),
  ?assertEqual(1, to_bcounter(k, 30, 31, ?SMALLER_TOKEN)),
  ?assertThrow(_, to_bcounter(k, 6, 5, ?SMALLER_TOKEN)),
  ?assertThrow(_, to_bcounter(k, 5, 5, ?SMALLER_TOKEN)).

from_bcounter_test() ->
  % greater than
  ?assertEqual(31, from_bcounter(?GREATER_TOKEN, 1, 30)),
  % smaller than
  ?assertEqual(4, from_bcounter(?SMALLER_TOKEN, 1, 5)).

-endif.
