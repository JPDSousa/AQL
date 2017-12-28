
-module(ipa).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("parser.hrl").
-include("aql.hrl").

-export([new/0,
          touch/0, touch_cascade/0, insert/0, delete/0, delete_cascade/0,
          is_visible/1, is_visible/2,
          status/2]).

new() ->
  insert().

touch() -> t.
touch_cascade() -> tc.
insert() -> i.
delete() -> d.
delete_cascade() -> dc.

is_visible(ExState) ->
  is_visible(ExState, []).

is_visible(_, [dc | _ImplicitState]) -> false;
is_visible(ExplicitState, [tc | ImplicitState]) ->
  is_visible(ExplicitState, ImplicitState);
is_visible(i, []) -> true;
is_visible(t, []) -> true;
is_visible(d, []) -> false;
is_visible(_InvalidE, _InvalidI) -> 
  err.

status(_Rule, []) -> [];
status(Rule, [H | T]) ->
  status(Rule, T, H).

status(Heu, [H | T], Current) ->
  Res = merge(Heu, H, Current),
  status(Heu, T, Res);
status(_Heu, [], Current) ->
  Current.

% if X and Y are the same, both prevail
merge(_, X, X) -> X;
% if X is found first, Y prevails
merge([X | _T], X, Y) -> Y;
% if Y is found first, X prevails
merge([Y | _T], X, Y) -> X;
% if Z is not X or Y, find next
merge([_Z | T], X, Y) -> merge(T, X, Y);
% if both not found, error
merge([], _X, _Y) -> err.

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).
new_test() ->
  ?assertEqual(i, new()).

is_visible_ok_test() ->
  ?assertEqual(is_visible(i, [tc, tc]), true),
  ?assertEqual(is_visible(i, [dc, tc]), false),
  ?assertEqual(is_visible(t, [tc, tc]), true),
  ?assertEqual(is_visible(t, [dc, tc]), false),
  ?assertEqual(is_visible(d, [tc, tc]), false),
  ?assertEqual(is_visible(d, [dc, tc]), false).

is_visible_err_test() ->
  ?assertEqual(is_visible(random_value, random_value), err).

status_test() ->
  List1 = [d],
  List2 = [i, d, tc],
  List3 = [d, d, dc],
  List4 = [i, d, d],
  ?assertEqual(status([d, i, tc, dc, t], List1), d),
  ?assertEqual(status([d, tc, i, t, dc], List2), i),
  ?assertEqual(status([dc, d, tc, t, i], List3), d),
  ?assertEqual(status([tc, dc, t, d, i], List4), i).

-endif.
