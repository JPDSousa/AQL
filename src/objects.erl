%% @author joao
%% @doc @todo Add description to objects.

-module(objects).

-export([create_add_all/2,
		 		create_remove_all/2]).

-export([create_field_map_op/3,
				create_map_update/2,
				create_single_map_update/4]).

-export([increment_counter/1,
				decrement_counter/1]).

-export([set_integer/1]).

-export([assign_lww/1]).

-export([create_bound_object/3,
				create_op/3]).

%% ====================================================================
%% Crdt_Set functions
%% ====================================================================

create_add_all(BoundObject, Entries) when is_list(Entries) ->
	create_op(BoundObject, add_all, Entries);
create_add_all(BoundObject, Entry) ->
	create_add_all(BoundObject, [Entry]).

create_remove_all(BoundObject, Entries) when is_list(Entries) ->
	create_op(BoundObject, remove_all, Entries);
create_remove_all(BoundObject, Entry) ->
 	create_remove_all(BoundObject, [Entry]).

%% ====================================================================
%% Crdt_map functions
%% ====================================================================

create_field_map_op(Key, Type, Op) ->
	{{Key, Type}, Op}.

create_map_update(BoundObject, ListOps) when is_list(ListOps) ->
	{BoundObject, update, ListOps};
create_map_update(BoundObject, Op) ->
	create_map_update(BoundObject, [Op]).

create_single_map_update(BoundObject, FieldKey, FieldType, FieldOp) ->
	FieldUpdate = create_field_map_op(FieldKey, FieldType, FieldOp),
	create_map_update(BoundObject, FieldUpdate).

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

create_bound_object(Key, CrdtType, Bucket) when is_atom(Key) and is_atom(CrdtType) and is_atom(Bucket) ->
	{Key, CrdtType, Bucket}.
