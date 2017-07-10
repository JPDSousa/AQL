
-module(bcounter).

-include_lib("parser.hrl").

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
