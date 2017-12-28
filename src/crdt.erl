%% @author JPDSousa
%% @doc logic for handling conflict free resolution policies

-module(crdt).

-include_lib("parser.hrl").
-include_lib("aql.hrl").

-export([add_all/1, add_all/2,
		 remove_all/1, remove_all/2]).

-export([field_map_op/3, field_map_op/2,
				map_update/2,
				single_map_update/3, single_map_update/4]).

-export([increment_counter/1,
				decrement_counter/1]).

-export([set_integer/1]).

-export([assign_lww/1]).

-export([enable_flag/0,
				disable_flag/0]).

-export([create_bound_object/3,
				create_op/3]).

-export([ipa_update/2]).

%% ====================================================================
%% Crdt_Set functions
%% ====================================================================

add_all([]) -> ?IGNORE_OP;
add_all(Entries) when is_list(Entries) ->
	{add_all, Entries};
add_all(Entry) ->
	{add, Entry}.

add_all(_, []) -> ?IGNORE_OP;
add_all(BoundObjects, Entries) when is_list(BoundObjects) and is_list(Entries) ->
	lists:map(fun (B) ->
		add_all(B, Entries)
	end, BoundObjects);
add_all(BoundObject, Entries) when is_list(Entries) ->
	{BoundObject, add_all, Entries};
add_all(BoundObject, Entry) ->
	{BoundObject, add, Entry}.

remove_all([]) -> ?IGNORE_OP;
remove_all(Entries) when is_list(Entries) ->
	{remove_all, Entries};
remove_all(Entry) ->
 	{remove, Entry}.

remove_all(_, []) -> ?IGNORE_OP;
remove_all(BoundObjects, Entries) when is_list(BoundObjects) and is_list(Entries) ->
	lists:map(fun (B) ->
		remove_all(B, Entries)
	end, BoundObjects);
remove_all(BoundObject, Entries) when is_list(Entries) ->
	create_op(BoundObject, remove_all, Entries);
remove_all(BoundObject, Entry) ->
	create_op(BoundObject, remove, Entry).

%% ====================================================================
%% Crdt_map functions
%% ====================================================================

field_map_op(Key, Type, Op) ->
	field_map_op(?MAP_KEY(Key, Type), Op).

field_map_op(_Key, ?IGNORE_OP) -> ?IGNORE_OP;
field_map_op(Key, Op) ->
	{Key, Op}.

map_update(_, []) -> ?IGNORE_OP;
map_update(BoundObjects, ListOps) when is_list(BoundObjects) and is_list(ListOps) ->
	lists:map(fun (B) ->
		map_update(B, ListOps)
	end, BoundObjects);
map_update(BoundObject, ListOps) when is_list(ListOps) ->
	{BoundObject, update, ListOps};
map_update(BoundObject, Op) ->
	map_update(BoundObject, [Op]).

single_map_update(BoundObject, FieldKey, FieldType, FieldOp) ->
	FieldUpdate = field_map_op(FieldKey, FieldType, FieldOp),
	map_update(BoundObject, FieldUpdate).

single_map_update(BoundObject, Key, FieldOp) ->
	FieldUpdate = field_map_op(Key, FieldOp),
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
%% Flag functions
%% ====================================================================

enable_flag() ->
	{enable, {}}.

disable_flag() ->
	{disable, {}}.

%% ====================================================================
%% Bounded counter functions
%% ====================================================================

increment_counter(0) -> ?IGNORE_OP;
increment_counter(Value) when is_integer(Value) ->
	bcounter_op(increment, Value).

decrement_counter(0) -> ?IGNORE_OP;
decrement_counter(Value) when is_integer(Value) ->
	bcounter_op(decrement, Value).

bcounter_op(Op, Value) ->
	{Op, {Value, term}}.

%% ====================================================================
%% Ipa functions
%% ====================================================================

ipa_update(Keys, State) when is_list(Keys) ->
	lists:map(fun (K) -> ipa_update(K, State) end, Keys);
ipa_update(Key, State) ->
	Op = crdt:assign_lww(State),
	crdt:single_map_update(Key, element:st_key(), Op).

%% ====================================================================
%% Utility functions
%% ====================================================================

create_op(BoundObject, Operation, OpParam) ->
	{BoundObject, Operation, OpParam}.

create_bound_object(Key, CrdtType, Bucket) ->
	BucketAtom = utils:to_atom(Bucket),
	?BOUND_OBJECT(Key, CrdtType, BucketAtom).
