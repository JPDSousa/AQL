%% @author Joao
%% @doc @todo Add description to select.


-module(insert).

-define(NO_PK, none).

-include("aql.hrl").
-include("parser.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/3]).

exec({Table, Tables}, Props, TxId) ->
	Keys = get_keys(Table, Props),
	Values = proplists:get_value(?PROP_VALUES, Props),
	AnnElement = element:new(Table),
	{ok, Element} = element:put(Keys, Values, AnnElement, Tables, TxId),
	element:insert(Element, TxId),
	Pk = element:primary_key(Element),
	index:put(Pk, TxId),
	% update foreign key references
	touch_cascade(Element, TxId),
	Fks = element:foreign_keys(foreign_keys:from_table(Table), Element),
	lists:foreach(fun (Fk) -> touch(Fk, Tables, TxId) end, Fks).

%% ====================================================================
%% Internal functions
%% ====================================================================

touch({_Col, {PTabName, _PTabAttr}, Value}, Tables, TxId) ->
	TKey = element:create_key(Value, PTabName),
	% TODO check if exists
	antidote:update_objects(crdt:ipa_update(TKey, ipa:touch()), TxId),
	{ok, [Element]} = antidote:read_objects(TKey, TxId),
	Table = table:lookup(PTabName, Tables),
	% touch cascade
	touch_cascade(Element, Table, PTabName, TxId),
	% touch parents
	Fks = element:foreign_keys(foreign_keys:from_table(Table), Element, PTabName),
	lists:foreach(fun (K) -> touch(K, Tables, TxId) end, Fks).

touch_cascade(Element, TxId) when is_tuple(Element) ->
	Data = element:data(Element),
	Cols = element:attributes(Element),
	TName = element:table(Element),
	touch_cascade(Data, Cols, TName, TxId).

touch_cascade(Data, Table, TName, TxId) when is_list(Table) ->
	Cols = column:s_from_table(Table),
	touch_cascade(Data, Cols, TName, TxId);
touch_cascade(Data, Cols, TName, TxId) ->
	Fks = foreign_keys:from_columns(Cols),
	ShadowCols = foreign_keys:shadow_cols(Data),
	FkOps = lists:map(fun({{FkName, AQL}, _Parent}) ->
		Value = element:get(FkName, types:to_crdt(AQL), Data, TName),
		index:tag(TName, FkName, Value, ipa:touch_cascade())
	end, Fks),
	ShadowOps = lists:map(fun({{K, _Type}, V}) ->
		index:tag(TName, K, V, ipa:touch_cascade())
	end, ShadowCols),
	Ops = lists:append(FkOps, ShadowOps),
	case Ops of
		[] -> ok;
		_Else ->
			antidote:update_objects(Ops, TxId)
	end.

get_keys(Table, Props) ->
	Clause = proplists:get_value(?PROP_COLUMNS, Props),
	case Clause of
		?PARSER_WILDCARD ->
			Keys = column:s_names(Table),
			lists:map(fun (V) -> ?PARSER_ATOM(V) end, Keys);
		_Else ->
			Clause
	end.
