%% @author Joao
%% @doc @todo Add description to select.


-module(select).

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/2]).

exec(Table, Select) ->
	{ok, TName} = tables:name(Table),
	{ok, _Projection} = query_utils:search_clause(keys, Select),
	{ok, Condition} = query_utils:search_clause(where, Select),
	scan(TName, Condition, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

scan(TableName, [{{atom_value, _Left}, Arop, {string, Str}} | Tail], Acc) ->
	case Arop of
		{assignment, "="} ->
			NewAcc = lists:append(Acc, element:new(Str, TableName)),
			scan(TableName, Tail, NewAcc);
		_Else ->
			{err, "Not supported yet! :)"}
	end;
scan(_TName, [], Acc) ->
	{ok, Results, _CT} = antidote:read_objects(Acc),
	{ok, Results}.
