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
	{ok, Element} = element:put(Keys, Values, AnnElement),
	element:insert(Element, TxId),
	Pk = element:primary_key(Element),
	index:put(Pk, TxId),
	% update foreign key references
	TName = table:name(Table),
	Fks = element:foreign_keys(Element),
	lists:foreach(fun (Fk) -> touch(Pk, Fk, TxId) end, Fks).

%% ====================================================================
%% Internal functions
%% ====================================================================

touch({_K, _T, CTabName} = Pk, {CName, {Value, PTabName}}, TxId) ->
	TKey = element:create_key(Value, PTabName),
	% TODO check if exists
	antidote:update_objects(crdt:ipa_update(TKey, ipa:touch()), TxId),
	{ok, [Element]} = antidote:read_objects(TKey, TxId),
	% touch cascade children
	%Refs = proplists:get_value(element:refs_key(), Element, []),
	%lists:foreach(fun (K) -> touch_cascade(K, TxId) end, Refs),
	% touch parents
	Table = table:lookup(PTabName, TxId),
	FKs = foreign_keys:from_table(Table),
	lists:foreach(fun (K) -> touch(TKey, K, TxId) end, foreign_keys:parents(Element, FKs)).

touch_cascade(Key, TxId) ->
	antidote:update_objects(crdt:ipa_update(Key, ipa:touch_cascade()), TxId),
	{ok, [Element]} = antidote:read_objects(Key, TxId),
	Refs = proplists:get_value(element:refs_key(), Element, []),
	lists:foreach(fun (K) -> touch_cascade(K, TxId) end, Refs).

get_keys(Table, Props) ->
	Clause = proplists:get_value(?PROP_COLUMNS, Props),
	case Clause of
		?PARSER_WILDCARD ->
			Keys = table:get_col_names(Table),
			lists:map(fun (V) -> ?PARSER_ATOM(V) end, Keys);
		_Else ->
			Clause
	end.
