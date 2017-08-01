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
	% TODO touch cascade children
	% touch parents
	Table = table:lookup(PTabName, Tables),
	Fks = element:foreign_keys(foreign_keys:from_table(Table), Element, PTabName),
	lists:foreach(fun (K) -> touch(K, Tables, TxId) end, Fks).

touch_cascade(Key, TxId) ->
	antidote:update_objects(crdt:ipa_update(Key, ipa:touch_cascade()), TxId),
	{ok, [Element]} = antidote:read_objects(Key, TxId),
	Refs = proplists:get_value(element:refs_key(), Element, []),
	lists:foreach(fun (K) -> touch_cascade(K, TxId) end, Refs).

get_keys(Table, Props) ->
	Clause = proplists:get_value(?PROP_COLUMNS, Props),
	case Clause of
		?PARSER_WILDCARD ->
			Keys = column:s_names(Table),
			lists:map(fun (V) -> ?PARSER_ATOM(V) end, Keys);
		_Else ->
			Clause
	end.
