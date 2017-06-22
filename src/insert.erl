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
	Keys = query_utils:search_clause(?PROP_COLUMNS, Props),
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

update_ref(ParentKey, ChildKey, TxId) ->
	{RefKey, RefType} = element:ref_keys(),
	RefOp = crdt:add_all(ChildKey),
	Update = crdt:create_single_map_update(ParentKey, RefKey, RefType, RefOp),
	antidote:update_objects(Update, TxId).
