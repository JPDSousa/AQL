%% @author joao
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([testImpl1/0, testImpl2/0]).

testImpl1() ->
	%Updates = create_counterUpdate(500, []),
	%update_objects(ignore, [], Updates),
	Reads = create_counterRead(500, []),
	{ok, Result, _Commit} = antidote:read_objects(ignore, [], Reads),
	Result.

testImpl2() ->
	Updates = create_maps(3, []),
	io:fwrite("~p~n", [Updates]),
	antidote:update_objects(ignore, [], Updates).

%% ====================================================================
%% Internal functions
%% ====================================================================


create_maps(Value, Objects) ->
	List = integer_to_list(Value),
	List1 = lists:append("map", List),
	Map = {list_to_atom(List1), antidote_crdt_map, bucket},
	if
		Value > 0 ->
			Updates = create_map_updates(),
			{Map, {update, {Updates, antidote:antidote_node()}}};
			%create_maps(Value-1, lists:flatten([Objects, {Map, {update, Updates}}]));
		true ->
			Objects
	end.

create_map_updates() ->
	U1 = {update, {{f1, riak_dt_lwwreg}, {[{}]}}, {assign, 1}},
	U2 = {update, "f2", {assign, 1}},
	U3 = {update, "f3", {assign, 1}},
	U4 = {update, "f4", {assign, 1}},
	[U1].

create_counterUpdate(Value, Objects) ->
	List = integer_to_list(Value),
	Counter = {list_to_atom(List), antidote_crdt_counter, bucket},
	if
		Value > 300 ->
			create_counterUpdate(Value-1, lists:flatten([Objects, {Counter, increment, 1}]));
		true ->
			Objects
	end.

create_counterRead(Value, Objects) ->
	List = integer_to_list(Value),
	Counter = {list_to_atom(List), antidote_crdt_counter, bucket},
	if
		Value > 300 ->
			create_counterRead(Value-1, lists:flatten([Objects, Counter]));
		true ->
			Objects
	end.