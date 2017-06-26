
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
  Updates = crdt:map_update(Keys, crdt:assign_lww()),
  antidote:update_objects(Updates).
