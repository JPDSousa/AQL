
-module(ipa).

-define(CREATE(Token, Visible), {Token, Visible}).

-define(INSERT, ?CREATE(i, true)).
-define(TOUCH, ?CREATE(t, true)).
-define(TOUCH_CASCADE, ?CREATE(tc, true)).
-define(DELETE, ?CREATE(d, false)).
-define(DELETE_CASCADE, ?CREATE(dc, false)).

-define(ADD_WINS, add).
-define(REMOVE_WINS, remove).

-define(H_AW, [{dc, 1}, {tc, 2}, {d, 3}, {t, 4}, {i, 5}]).
-define(H_RW, [{tc, 1}, {t, 2}, {i, 3}, {dc, 4}, {d, 5}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0,
          is_visible/1, value/1,
          status/2,
          from_value/1]).

new() ->
  ?INSERT.

from_value(i) -> ?INSERT;
from_value(t) -> ?TOUCH;
from_value(tc) -> ?TOUCH_CASCADE;
from_value(d) -> ?DELETE;
from_value(dc) -> ?DELETE_CASCADE.

is_visible(?CREATE(_T, Visible)) ->
  Visible;
is_visible(_Invalid) ->
  err.

value(?CREATE(Token, _V)) ->
  Token;
value(_Invalid) ->
  err.

status(Mode, [H | T]) ->
  Heu = heu(Mode),
  status(Heu, T, H).

status(Heu, [H | T], Current) ->
  Res = merge(Heu, H, Current),
  status(Heu, T, Res);
status(_Heu, [], Current) ->
  Current.

heu(?ADD_WINS) -> ?H_AW;
heu(?REMOVE_WINS) -> ?H_RW;
heu(_Invalid) -> err.

merge(Heu, X, Y) ->
  TokenX = value(X),
  TokenY = value(Y),
  ScoreX = proplists:get_value(TokenX, Heu),
  ScoreY = proplists:get_value(TokenY, Heu),
  if
    ScoreX >= ScoreY ->
      X;
    true ->
      Y
  end.

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).
new_test() ->
  ?assertEqual(?INSERT, new()).

is_visible_ok_test() ->
  ?assertEqual(is_visible(?CREATE(a, true)), true).

is_visible_err_test() ->
  ?assertEqual(is_visible(random_value), err).

value_ok_test() ->
  ?assertEqual(value(?CREATE(a, b)), a).

value_err_test() ->
  ?assertEqual(value(random_value), err).

status_test() ->
  List1 = [?DELETE],
  List2 = [?INSERT, ?DELETE, ?TOUCH_CASCADE],
  List3 = [?DELETE, ?DELETE, ?DELETE_CASCADE],
  List4 = [?INSERT, ?DELETE, ?DELETE],
  ?assertEqual(status(?ADD_WINS, List1), ?DELETE),
  ?assertEqual(status(?ADD_WINS, List2), ?INSERT),
  ?assertEqual(status(?ADD_WINS, List3), ?DELETE),
  ?assertEqual(status(?ADD_WINS, List4), ?INSERT).

-endif.
