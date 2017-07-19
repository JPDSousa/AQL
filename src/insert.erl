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

exec(Table, Props, TxId) ->
	Keys = get_keys(Table, Props),
	Values = proplists:get_value(?PROP_VALUES, Props),
	AnnElement = element:new(Table),
	{ok, Element} = element:put(Keys, Values, AnnElement),
	elemet:insert(Element, TxId),
	index:put(element:primary_key(Element), TxId),
	% update foreign key references
	%Pk = element:primary_key(Element),
	Fks = element:foreign_keys(Element),
	lists:foreach(fun (Fk) -> touch(Fk, TxId) end, Fks).
	%lists:foreach(fun (Fk) -> add_ref(Fk, Pk, TxId) end, Fks).

%% ====================================================================
%% Internal functions
%% ====================================================================

add_ref(Key, Ref, TxId) ->
	Op = crdt:single_map_update(Key, element:refs_key(), crdt:add_all(Ref)),
	antidote:update_objects(Op, TxId).

touch({_ID, _Type, TName} = Key, TxId) ->
	antidote:update_objects(crdt:ipa_update(Key, ipa:touch()), TxId),
	{ok, [Element]} = antidote:read_objects(Key, TxId),
	% touch cascade children
	Refs = proplists:get_value(element:refs_key(), Element, []),
	lists:foreach(fun (K) -> touch_cascade(K, TxId) end, Refs),
	% touch parents
	Table = table:lookup(TName, TxId),
	FKs = foreign_keys:from_table(Table),
	lists:foreach(fun (K) -> touch(K, TxId) end, foreign_keys:parents(Element, FKs)).

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
