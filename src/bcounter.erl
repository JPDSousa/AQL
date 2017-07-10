
-module(bcounter).

-include_lib("parser.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([to_bcounter/4,
        from_bcounter/3]).

to_bcounter(Key, Value, Offset, Comp) ->
  OffValue = apply_offset_value(Comp, Offset, Value),
  check_bcounter_value(Key, OffValue).

check_bcounter_value(_Key, Value) when Value > 0 -> Value;
check_bcounter_value(Key, Value) -> throw(lists:concat(["Invalid value ", Value, " for column ", Key])).

apply_offset_value(?GREATER_KEY, Offset, Value) -> Value-Offset;
apply_offset_value(?SMALLER_KEY, Offset, Value) -> Offset-Value.

from_bcounter(?GREATER_KEY, Value, Offset) -> Value+Offset;
from_bcounter(?SMALLER_KEY, Value, Offset) -> Offset-Value.

%%====================================================================
%% Eunit tests
%%====================================================================

to_bcounter_test() ->
  % greater than
  ?assertEqual(5, to_bcounter(k, 5, 0, ?GREATER_KEY)),
  ?assertEqual(3, to_bcounter(k, 5, 2, ?GREATER_KEY)),
  ?assertEqual(1, to_bcounter(k, 31, 30, ?GREATER_KEY)),
  ?assertThrow(_, to_bcounter(k, 5, 6, ?GREATER_KEY)),
  ?assertThrow(_, to_bcounter(k, 5, 5, ?GREATER_KEY)),
  % smaller than
  ?assertEqual(5, to_bcounter(k, 0, 5, ?SMALLER_KEY)),
  ?assertEqual(3, to_bcounter(k, 2, 5, ?SMALLER_KEY)),
  ?assertEqual(1, to_bcounter(k, 30, 31, ?SMALLER_KEY)),
  ?assertThrow(_, to_bcounter(k, 6, 5, ?SMALLER_KEY)),
  ?assertThrow(_, to_bcounter(k, 5, 5, ?SMALLER_KEY)).

from_bcounter_test() ->
  % greater than
  ?assertEqual(31, from_bcounter(?GREATER_KEY, 1, 30)),
  % smaller than
  ?assertEqual(4, from_bcounter(?SMALLER_KEY, 1, 5)).
