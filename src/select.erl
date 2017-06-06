%% @author Joao
%% @doc @todo Add description to select.


-module(select).

-include("parser.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/2]).

exec(Table, Select) ->
	TName = table:name(Table),
	Projection = query_utils:search_clause(?PROP_COLUMNS, Select),
	% validate projection fields
	Condition = query_utils:search_clause(?WHERE_TOKEN, Select),
	Keys = where:scan(TName, Condition),
	{ok, Results, _CT} = antidote:read_objects(Keys),
	ProjRes = project(Projection, table:get_columns(Table), Results, []),
	{ok, ProjRes}.

project(Projection, Columns, [Result | Results], Acc) ->
	ProjRes = project_row(Projection, Columns, dict:to_list(Result), dict:new()),
	NewAcc = lists:flatten([ProjRes], Acc),
	project(Projection, Columns, Results, NewAcc);
project(_Projection, _Columns, [], Acc) ->
	Acc.

project_row([?PARSER_ATOM(Atom) | Tail], Columns, Result, Acc) ->
	Column = dict:get(Atom, Columns),
	NewColumns = dict:erase(Atom, Columns),
	CType = column:type(Column),
	Field = dict:get({Atom, CType}, Result),
	NewResult = dict:erase({Atom, CType}, Result),
	NewAcc = dict:store(Atom, Field, Acc),
	project_row(Tail, NewColumns, NewResult, NewAcc);
project_row([], _Columns, _Result, Acc) ->
	Acc.
