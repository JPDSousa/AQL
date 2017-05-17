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
	{table, EfTable} = Table,
	{ok, Cls} = query_utils:search_clause(keys, EfTable),
	{ok, Keys} = where:scan(TName, Cls, Condition),
	{ok, Results, _CT} = antidote:read_objects(Keys),
	{ok, Results}.
