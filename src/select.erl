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
	% TODO validate projection fields
	Condition = query_utils:search_clause(?WHERE_TOKEN, Select),
	Keys = where:scan(TName, Condition),
	{ok, Results, _CT} = antidote:read_objects(Keys),
	ProjRes = project(Projection, Results, []),
	{ok, ProjRes}.

project(Projection, [Result | Results], Acc) ->
	FormattedResult = lists:map(fun remove_type_map/1, Result),
	ProjRes = project_row(Projection, FormattedResult, []),
	NewAcc = lists:flatten([ProjRes], Acc),
	project(Projection, Results, NewAcc);
project(_Projection, [], Acc) ->
	Acc.

project_row(?PARSER_WILDCARD, Result, _Acc) ->
	Result;
project_row([?PARSER_ATOM(Atom) | Tail], Result, Acc) ->
	Field = proplists:get_value(Atom, Result, undefined),
	NewResult = proplists:delete(Atom, Result),
	NewAcc = Acc ++ [{Atom, Field}],
	project_row(Tail, NewResult, NewAcc);
project_row([], _Result, Acc) ->
	Acc.

remove_type_map({{CName, _CType}, Value}) ->
	{CName, Value}.
