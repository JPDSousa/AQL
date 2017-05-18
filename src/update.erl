
-module(update).

-include("aql.hrl").

-export([exec/2]).

exec(Table, Props) ->
  {ok, TName} = tables:name(Table),
  {table, EfTable} = Table,
  {ok, Cls} = query_utils:search_clause(keys, EfTable),
  {ok, SetClause} = query_utils:search_clause(set, Props),
  {ok, WhereClause} = query_utils:search_clause(where, Props),
  {ok, FieldUpdates} = create_update(Table, [], SetClause),
  {ok, Keys} = where:scan(TName, Cls, WhereClause),
  MapUpdates = create_map_updates([], Keys, FieldUpdates),
  {ok, _CT} = antidote:update_objects(MapUpdates),
  ok.

create_map_updates(Acc, [Key | Tail], Updates) ->
  MapUpdate = objects:create_map_update(Key, Updates),
  create_map_updates(lists:append(Acc, MapUpdate), Tail, Updates);
create_map_updates(Acc, [], _Updates) ->
  Acc.

create_update(Table, Acc, [{{atom_value, CollumnName}, Op, OpParam} | Tail]) ->
  {ok, Collumn} = tables:collumn_metadata(Table, CollumnName),
  {ok, Update} = resolve_op(Collumn, Op, OpParam),
  create_update(Table, lists:append(Acc, Update), Tail);
create_update(_Table, Acc, []) ->
  {ok, Acc}.

resolve_op(Collumn, {assign, _TokenChars}, {_TokenType, Value}) ->
  {ok, CollumnType} = tables:key_type(Collumn),
  {ok, CollumnName} = tables:key_name(Collumn),
  case CollumnType of
    ?AQL_VARCHAR ->
      Op = objects:assign_lww(Value),
      Update = objects:create_field_map_op(CollumnName, ?CRDT_VARCHAR, Op),
      {ok, Update};
    _Else ->
      resolve_fail(CollumnName, CollumnType)
  end.

resolve_fail(CName, CType) ->
  Msg = string:concat(["Cannot assign to column ", CName, " of type ", CType]),
  {err, Msg}.
