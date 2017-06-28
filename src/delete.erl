
-module(delete).

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/2]).

-include("parser.hrl").

exec(Table, Props) ->
	Condition = query_utils:search_clause(?WHERE_TOKEN, Props),
	FKs = table:filter_fks(table:get_columns(Table)),
	Keys = where:scan(Table, Condition),
  {ok, TxId} = antidote:start_transaction(),
  lists:for_each(fun (K) -> remove_from_parent(FKs, K, TxId) end, Keys),
  antidote:update_objects(create_update_op(Keys), TxId),
	antidote:commit_transaction(TxId).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_update_op(Keys) ->
	StKey = element:st_key(),
	StOp = crdt:field_map_op(StKey, crdt:assign_lww(ipa:delete())),
	crdt:map_update(Keys, StOp).

remove_from_parent(FKs, ChildKey, TxId) ->
	ParentKeys = get_parents(FKs, ChildKey, TxId),
	RefsKey = element:refs_key(),
	RefOp = crdt:field_map_op(RefsKey, crdt:remove_all(ChildKey)),
	Updates = lists:map(fun (P) -> crdt:update_map(P, RefOp) end, ParentKeys),
	antidote:update_objects(Updates, TxId).

get_parents(FKs, Key, TxId) ->
	Res = antidote:read_objects(Key, TxId),
	case Res of
		{ok, [Row]} ->
			lists:map(fun ({K, V}) ->
				Value = proplists:get_value(K, Row),
				element:create_key(Value, V)
			end, FKs);
		_Else ->
			throw("No such element")
	end.
