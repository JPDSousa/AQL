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
	{ok, TxId} = antidote:start_transaction(),
	antidote:update_objects(AntidoteOp, TxId),
	% update foreign key references
	Pk = element:primary_key(Element),
	Fks = element:foreign_keys(Element),
	lists:foreach(fun (Fk) -> update_ref(Fk, Pk, TxId) end, Fks),
	% end
	Res = antidote:commit_transaction(TxId),
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

get_child(ParentKey, TxId) ->
	{ok, [Parent]} = antidote:read_objects(ParentKey, TxId),
	RefsKey = element:refs_key(),
	Children = proplists:get_value(RefsKey, Parent, []),
	io:fwrite("Parent: ~p~nChildren: ~p~n", [Parent, Children]),
	Children.

update_ref(ParentKey, ChildKey, TxId) ->
	% update child tree
	Children = get_child(ParentKey, TxId),
	lists:foreach(fun (C) -> update_child(C, TxId) end, Children),
	% update itself
	RefsKey = element:refs_key(),
	StKey = element:st_key(),
	RefOp = crdt:field_map_op(RefsKey, crdt:add_all(ChildKey)),
	StOp = crdt:field_map_op(StKey, crdt:assign_lww(ipa:touch())),
	Update = crdt:map_update(ParentKey, [StOp, RefOp]),
	antidote:update_objects(Update, TxId).

update_child(ParentKey, TxId) ->
	% update child tree
	Children = get_child(ParentKey, TxId),
	lists:foreach(fun (C) -> update_child(C, TxId) end, Children),
	% update itself
	StKey = element:st_key(),
	StOp = crdt:field_map_op(StKey, crdt:assign_lww(ipa:touch_cascade())),
	antidote:update_objects(crdt:map_update(ParentKey, StOp), TxId).
