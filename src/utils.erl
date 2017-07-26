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

-export([to_atom/1,
        to_list/1]).

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

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).



-endif.
