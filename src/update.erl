
-module(update).

-include("aql.hrl").

-export([exec/2]).

exec(Table, Props) ->
  {ok, SetClause} = query_utils:search_clause(set, Props),
  %TODO: add where clause
  {ok, FieldUpdates} = create_update(Table, [], SetClause),
  Keys = [element:new('Dummy', 'Student')],
  MapUpdates = create_map_updates([], Keys, FieldUpdates),
  {ok, _CT} = antidote:update_objects(MapUpdates),
  ok.

create_map_updates(Acc, [Key | Tail], Updates) ->
  MapUpdate = objects:create_map_update(Key, Updates),
  create_map_updates(lists:append(Acc, MapUpdate), Tail, Updates);
create_map_updates(Acc, [], _Updates) ->
  Acc.

create_update(Table, Acc, [{{atom_value, CollumnName}, _Expression} | Tail]) ->
  {ok, _Collumn} = tables:collumn_metadata(Table, CollumnName),
  Op = objects:decrement_counter(1),
  Update = objects:create_field_map_op('YearsLeft', ?CRDT_COUNTER_INT, Op),
  create_update(Table, lists:append(Acc, Update), Tail);
create_update(_Table, Acc, []) ->
  {ok, Acc}.
