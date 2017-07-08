
-module(delete).

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/3]).

-include("parser.hrl").
-include_lib("eunit/include/eunit.hrl").

exec(Table, Props, TxId) ->
	TName = table:name(Table),
	Condition = proplists:get_value(?WHERE_TOKEN, Props),
	Keys = where:scan(TName, Condition),
	FKs = foreign_keys:from_table(Table),
  lists:foreach(fun (K) -> delete_cascade(FKs, K, TxId, ipa:delete()) end, Keys).

%% ====================================================================
%% Internal functions
%% ====================================================================

delete_cascade_tname(TName, Key, TxId) ->
	Table = table:lookup(TName, TxId),
	FKs = foreign_keys:from_table(Table),
	delete_cascade(FKs, Key, TxId, ipa:delete_cascade()).

delete_cascade(FKs, Key, TxId, State) ->
	remove_from_parents(FKs, Key, TxId),
	antidote:update_objects(crdt:ipa_update(Key, State), TxId),
	{ok, [Element]} = antidote:read_objects(Key, TxId),
	Refs = proplists:get_value(element:refs_key(), Element, []),
	lists:foreach(fun ({_Pk, _Map, TName} = K) ->
		delete_cascade_tname(TName, K, TxId)
	end, Refs).

remove_from_parents(FKs, Key, TxId) when is_list(FKs) ->
	Res = antidote:read_objects(Key, TxId),
	case Res of
		{ok, [Row]} ->
			Parents = foreign_keys:parents(Row, FKs),
			lists:foreach(fun (P) -> remove_ref(Key, P, TxId) end, Parents);
		_Else ->
			throw("No such element")
	end.

remove_ref(Ref, Key, TxId) ->
	RefsKey = element:refs_key(),
	Update = crdt:single_map_update(Key, RefsKey, crdt:remove_all(Ref)),
	antidote:update_objects(Update, TxId).
