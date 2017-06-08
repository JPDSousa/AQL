%% @author Joao
%% @doc @todo Add description to select.


-module(insert).

-define(NO_PK, none).

-include("aql.hrl").
-include("parser.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/2]).

exec(Table, Props) ->
	Keys = get_keys(Table, Props),
	Values = query_utils:search_clause(?PROP_VALUES, Props),
	AnnElement = element:new(Table),
	{ok, Element} = element:put(Keys, Values, AnnElement),
	AntidoteOp = element:create_db_op(Element),
	Res = antidote:update_objects(AntidoteOp),
	case Res of
		{ok, _CT} ->
			ok;
		_Else ->
			Res
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_keys(Table, Props) ->
	Clause = query_utils:search_clause(?PROP_COLUMNS, Props),
	case Clause of
		?PARSER_WILDCARD ->
			Keys = table:get_col_names(Table),
			lists:map(fun (V) -> ?PARSER_ATOM(V) end, Keys);
		_Else ->
			Clause
	end.
