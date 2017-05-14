%% @author Joao
%% @doc @todo Add description to select.


-module(insert).

-define(NO_PK, none).

-include("aql.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/2]).

exec(Table, Props) ->
	{ok, Keys} = query_utils:search_clause(keys, Props),
	{ok, Values} = query_utils:search_clause(values, Props),
	insert_into(Table, Keys, Values).

%% ====================================================================
%% Internal functions
%% ====================================================================

insert_into(Table, Keys, Values) ->
	insert_into(Table, [], Keys, Values, ?NO_PK).

insert_into(Table, List, [{atom_value, Key}|OtherKeys], [Value|OtherValues], Pk) ->
	{ok, KeyMetadata} = tables:collumn_metadata(Table, Key),
	{ok, PrimaryKey} = tables:primary_key(Table),
	{ok, Update} = create_update(Key, KeyMetadata, Value),
	NewList = lists:append(List, [Update]),
	case Key of
		PrimaryKey ->
			{_LeexToken, EfValue} = Value,
			insert_into(Table, NewList, OtherKeys, OtherValues, EfValue);
		_Else ->
			insert_into(Table, NewList, OtherKeys, OtherValues, Pk)
	end;
insert_into(_Table, _List, [], [], ?NO_PK) ->
	{err, "No primary key found"};
insert_into(Table, List, [], [], Pk) ->
	ElementKey = create_key_name(Table, Pk),
	Element = objects:create_map_update(ElementKey, List),
	{ok, _CT} = antidote:update_objects(Element),
	ok.

create_key_name(Table, Pk) ->
	{ok, TName} = tables:name(Table),
	element:new(Pk, TName).

create_update(KeyName, KeyMetadata, Value) when is_list(KeyMetadata) ->
	{ok, AqlType} = tables:key_type(KeyMetadata),
	create_update(KeyName, AqlType, Value);
%integer
create_update(Name, ?AQL_INTEGER, {LeexToken, Value}) ->
	case LeexToken of
		number ->
			Op = objects:set_integer(Value),
			Update = objects:create_field_map_op(Name, ?CRDT_INTEGER, Op),
			{ok, Update};
		_Else ->
			ErrMsg = throwInvalidType(LeexToken, Name),
			{err, ErrMsg}
	end;
%varchar
create_update(Name, ?AQL_VARCHAR, {LeexToken, Value}) ->
	case LeexToken of
		string ->
			Op = objects:assign_lww(Value),
			Update = objects:create_field_map_op(Name, ?CRDT_VARCHAR, Op),
			{ok, Update};
		_Else ->
			ErrMsg = throwInvalidType(LeexToken, Name),
			{err, ErrMsg}
	end;
%counter int
create_update(Name, ?AQL_COUNTER_INT, {LeexToken, Value}) ->
	case LeexToken of
		number ->
			Op = objects:increment_counter(Value),
			Update = objects:create_field_map_op(Name, ?CRDT_COUNTER_INT, Op),
			{ok, Update};
		_Else ->
			ErrMsg = throwInvalidType(LeexToken, Name),
			{err, ErrMsg}
	end.

throwInvalidType(Type, CollumnName) ->
	io:format("Invalid type ~p for collumn: ~p", [Type, CollumnName]).
