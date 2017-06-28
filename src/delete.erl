
-module(delete).

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/2]).

exec(Table, Props) ->
	Condition = query_utils:search_clause(?WHERE_TOKEN, Select),
	Keys = where:scan(TName, Condition),
  StKey = element:st_key(),
  StOp = crdt:field_map_op(StKey, crdt:assign_lww(ipa:delete())),
  {ok, TxId} = antidote:start_transaction(),
  remove_from_parent(),
  Updates = crdt:map_update(Keys, crdt:assign_lww()),
  antidote:update_objects(Updates).

%% ====================================================================
%% Internal functions
%% ====================================================================

remove_from_parent() ->
	RefsKey = element:refs_key(),
	RefOp = crdt:field_map_op(RefsKey, crdt:add_all(ChildKey)),
