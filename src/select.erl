%% @author Joao
%% @doc @todo Add description to select.


-module(select).

-include("parser.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/2]).

exec(Table, Select) ->
	{ok, _Projection} = query_utils:search_clause(?PROP_COLUMNS, Select),
	{ok, Condition} = query_utils:search_clause(?WHERE_TOKEN, Select),
	Keys = where:scan(Table, Condition),
	{ok, Results, _CT} = antidote:read_objects(Keys),
	{ok, Results}.
