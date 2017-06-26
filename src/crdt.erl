%% @author joao
%% @doc @todo Add description to objects.

-module(crdt).

-include("aql.hrl").

-export([add_all/1,
		 		remove_all/1]).

-export([field_map_op/3, field_map_op/2,
				map_update/2,
				single_map_update/4]).

-export([increment_counter/1,
				decrement_counter/1]).

-export([set_integer/1]).

-export([assign_lww/1]).

-export([create_bound_object/3,
				create_op/3]).

%% ====================================================================
%% Crdt_Set functions
%% ====================================================================

add_all(Entries) when is_list(Entries) ->
	{add_all, Entries};
add_all(Entry) ->
	{add, Entry}.

remove_all(Entries) when is_list(Entries) ->
	{remove_all, Entries};
remove_all(Entry) ->
 	{remove, Entry}.

%% ====================================================================
%% Crdt_map functions
%% ====================================================================

field_map_op(Key, Type, Op) ->
	field_map_op({Key, Type}, Op).

field_map_op(Key, Op) ->
	{Key, Op}.

map_update(BoundObjects, ListOps) when is_list(BoundObjects) and is_list(ListOps) ->
	lists:for_each(fun (B) ->
		map_update(B, ListOps)
	end, BoundObjects).
map_update(BoundObject, ListOps) when is_list(ListOps) ->
	{BoundObject, update, ListOps};
map_update(BoundObject, Op) ->
	map_update(BoundObject, [Op]).

single_map_update(BoundObject, FieldKey, FieldType, FieldOp) ->
	FieldUpdate = field_map_op(FieldKey, FieldType, FieldOp),
	map_update(BoundObject, FieldUpdate).

%% ====================================================================
%% Integer functions
%% ====================================================================

set_integer(Value) when is_integer(Value) ->
	{set, Value}.

%% ====================================================================
%% Lwwreg functions
%% ====================================================================

assign_lww(Value) ->
	{assign, Value}.

%% ====================================================================
%% Bounded counter functions
%% ====================================================================

increment_counter(Value) when is_integer(Value) ->
	bcounter_op(increment, Value).

decrement_counter(Value) when is_integer(Value) ->
	bcounter_op(decrement, Value).

bcounter_op(Op, Value) ->
	{Op, {Value, term}}.

%% ====================================================================
%% Utility functions
%% ====================================================================

create_op(BoundObject, Operation, OpParam) ->
	{BoundObject, Operation, OpParam}.

create_bound_object(Key, Crdt, Bucket) when is_integer(Key) and ?is_crdt(Crdt) and ?is_dbbucket(Bucket) ->
	create_bound_object(integer_to_list(Key), Crdt, Bucket);
create_bound_object(Key, Crdt, Bucket) when is_list(Key) and ?is_crdt(Crdt) and ?is_dbbucket(Bucket) ->
	create_bound_object(list_to_atom(Key), Crdt, Bucket);
create_bound_object(Key, CrdtType, Bucket) when ?is_dbkey(Key) and ?is_crdt(CrdtType) and ?is_dbbucket(Bucket) ->
	{Key, CrdtType, Bucket}.
