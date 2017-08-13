%atom_to_binary/2, atom_to_list/1, binary_to_atom/2, binary_to_existing_atom/2,
%binary_to_list/1, bitstring_to_list/1, binary_to_term/1, float_to_list/1,
%fun_to_list/1, integer_to_list/1, integer_to_list/2, iolist_to_binary/1,
%iolist_to_atom/1, list_to_atom/1, list_to_binary/1, list_to_bitstring/1,
%list_to_existing_atom/1, list_to_float/1, list_to_integer/2, list_to_pid/1,
%list_to_tuple/1, pid_to_list/1, port_to_list/1, ref_to_list/1,
%term_to_binary/1, term_to_binary/2 and tuple_to_list/1.

-module(utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([to_atom/1, to_list/1,
        assert_same_size/3,
        list_to_dict/2,
        seek_and_destroy/2]).

to_atom(Term) when is_list(Term) ->
  list_to_atom(Term);
to_atom(Term) when is_integer(Term) ->
  List = integer_to_list(Term),
  list_to_atom(List);
to_atom(Term) when is_atom(Term) ->
  Term.

to_list(Term) when is_list(Term) ->
  Term;
to_list(Term) when is_integer(Term) ->
  integer_to_list(Term);
to_list(Term) when is_atom(Term) ->
  atom_to_list(Term).

assert_same_size(List1, List2, ErrMsg) ->
  if length(List1) =:= length(List2) -> ok;
    true -> throw(ErrMsg)
  end.

list_to_dict(List, KeyMapper) ->
	list_to_dict(List, KeyMapper, dict:new()).

list_to_dict([Value | T], KeyMapper, Acc) ->
	Key = KeyMapper(Value),
	NewMap = dict:store(Key, Value, Acc),
	list_to_dict(T, KeyMapper, NewMap);
list_to_dict([], _KeyMapper, Acc) ->
	Acc.

seek_and_destroy(Name, PropList) ->
  seek_and_destroy(Name, [], PropList).

seek_and_destroy(Name, Seen, [{Key, Value} | PropList]) ->
  case Name of
    Key ->
      {Value, lists:append(Seen, PropList)};
    _Else ->
      seek_and_destroy(Name, lists:append(Seen, [{Key, Value}]))
  end;
seek_and_destroy(_Name, Seen, []) ->
  {undefined, Seen}.

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

assert_same_size_test() ->
  ErrMsg = "Failing",
  ?assertEqual(ok, assert_same_size([1, 2], [3, 4], ErrMsg)),
  ?assertEqual(ok, assert_same_size([], [], ErrMsg)),
  ?assertThrow(_, assert_same_size([1, 2, 3], [], ErrMsg)).

-endif.
