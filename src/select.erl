%% @author Joao
%% @doc @todo Add description to select.


-module(select).

-include("parser.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/3]).

exec(Table, Select, TxId) ->
	TName = table:name(Table),
	Projection = proplists:get_value(?PROP_COLUMNS, Select),
	% TODO validate projection fields
	Condition = proplists:get_value(?WHERE_TOKEN, Select),
	Keys = where:scan(TName, Condition),
	{ok, Results} = antidote:read_objects(Keys, TxId),
	ProjRes = project(Projection, Results, []),
	{ok, ProjRes}.

project(Projection, [Result | Results], Acc) ->
	ProjRes = project_row(Projection, Result, []),
	NewAcc = lists:flatten([ProjRes], Acc),
	project(Projection, Results, NewAcc);
project(_Projection, [], Acc) ->
	Acc.

project_row(?PARSER_WILDCARD, Result, _Acc) ->
	Result;
project_row([?PARSER_ATOM(Atom) | Tail], Result, Acc) ->
	Field = get_value(Atom, Result),
	NewResult = proplists:delete(Atom, Result),
	NewAcc = Acc ++ [{Atom, Field}],
	project_row(Tail, NewResult, NewAcc);
project_row([], _Result, Acc) ->
	Acc.

get_value(Key, [{{Name, _Type}, _Value} = H| T]) ->
	case Key of
		Name ->
			H;
		_Else ->
			get_value(Key, T)
	end;
get_value(_Key, []) ->
	undefined.
