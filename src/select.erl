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
	_Projection = query_utils:search_clause(?PROP_COLUMNS, Select),
	Condition = query_utils:search_clause(?WHERE_TOKEN, Select),
	Keys = where:scan(TName, Condition),
	{ok, Results, _CT} = antidote:read_objects(Keys),
	{ok, Results}.
