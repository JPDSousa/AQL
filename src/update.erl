
-module(update).

-include("aql.hrl").
-include("parser.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([exec/3]).

%%====================================================================
%% API
%%====================================================================

exec(Table, Props, TxId) ->
  TName = table:name(Table),
  SetClause = proplists:get_value(?SET_TOKEN, Props),
  WhereClause = proplists:get_value(?WHERE_TOKEN, Props),
  FieldUpdates = create_update(Table, [], SetClause),
  Keys = where:scan(TName, WhereClause),
  MapUpdates = crdt:map_update(Keys, FieldUpdates),
  antidote:update_objects(MapUpdates, TxId).

%%====================================================================
%% Internal functions
%%====================================================================

create_update(Table, Acc, [{?PARSER_ATOM(ColumnName), Op, OpParam} | Tail]) ->
  {ok, Column} = table:get_column(Table, ColumnName),
  {ok, Update} = resolve_op(Column, Op, OpParam),
  create_update(Table, lists:flatten(Acc, [Update]), Tail);
create_update(_Table, Acc, []) ->
  Acc.

% varchar -> assign
resolve_op(Column, ?ASSIGN_OP(_TChars), ?PARSER_STRING(Value)) ->
  Op = fun crdt:assign_lww/1,
  resolve_op(Column, ?AQL_VARCHAR, ?CRDT_VARCHAR, Op, Value);
% integer -> assign
resolve_op(Column, ?ASSIGN_OP(_TChars), ?PARSER_NUMBER(Value)) ->
  Op = fun crdt:set_integer/1,
  resolve_op(Column, ?AQL_INTEGER, ?CRDT_INTEGER, Op, Value);
% counter -> increment
resolve_op(Column, ?INCREMENT_OP(_TChars), ?PARSER_NUMBER(Value)) ->
  Op = fun crdt:increment_counter/1,
  resolve_op(Column, ?AQL_COUNTER_INT, ?CRDT_COUNTER_INT, Op, Value);
% counter -> decrement
resolve_op(Column, ?DECREMENT_OP(_Tchars), ?PARSER_NUMBER(Value)) ->
  Op = fun crdt:decrement_counter/1,
  resolve_op(Column, ?AQL_COUNTER_INT, ?CRDT_COUNTER_INT, Op, Value).

resolve_op(Column, AqlType, CrdtType, Op, Value) ->
  CName = column:name(Column),
  CType = column:type(Column),
  case CType of
    AqlType ->
      Update = crdt:field_map_op(CName, CrdtType, Op(Value)),
      {ok, Update};
    _Else ->
      resolve_fail(CName, CType)
  end.

resolve_fail(CName, CType) ->
  Msg = string:concat(["Cannot assign to column ", CName, " of type ", CType]),
  {err, Msg}.


%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).
create_column_aux(CName, CType) ->
CAttrName = ?PROP_ATTR_NAME(?PARSER_ATOM(CName)),
CAttrType = ?ATTR_KEY(CType),
CAttrConstraint = ?PROP_ATTR_CONSTRAINT(?NO_CONSTRAINT),
{?PROP_ATTR, [CAttrName, CAttrType, CAttrConstraint]}.

resolve_op_varchar_test() ->
  CName = col1,
  CType = ?AQL_VARCHAR,
  Column = create_column_aux(CName, CType),
  Value = "Value",
  Expected = {ok, crdt:field_map_op(CName, ?CRDT_VARCHAR, crdt:assign_lww(Value))},
  Actual = resolve_op(Column, ?ASSIGN_OP("SomeChars"), ?PARSER_STRING(Value)),
  ?assertEqual(Expected, Actual).

resolve_op_integer_test() ->
  CName = col1,
  CType = ?AQL_INTEGER,
  Column = create_column_aux(CName, CType),
  Value = 2,
  Expected = {ok, crdt:field_map_op(CName, ?CRDT_INTEGER, crdt:set_integer(Value))},
  Actual = resolve_op(Column, ?ASSIGN_OP("SomeChars"), ?PARSER_NUMBER(Value)),
  ?assertEqual(Expected, Actual).

resolve_op_counter_increment_test() ->
  CName = col1,
  CType = ?AQL_COUNTER_INT,
  Column = create_column_aux(CName, CType),
  Value = 2,
  Expected = {ok, crdt:field_map_op(CName, ?CRDT_COUNTER_INT, crdt:increment_counter(Value))},
  Actual = resolve_op(Column, ?INCREMENT_OP("SomeChars"), ?PARSER_NUMBER(Value)),
  ?assertEqual(Expected, Actual).

resolve_op_counter_decrement_test() ->
  CName = col1,
  CType = ?AQL_COUNTER_INT,
  Column = create_column_aux(CName, CType),
  Value = 2,
  Expected = {ok, crdt:field_map_op(CName, ?CRDT_COUNTER_INT, crdt:decrement_counter(Value))},
  Actual = resolve_op(Column, ?DECREMENT_OP("SomeChars"), ?PARSER_NUMBER(Value)),
  ?assertEqual(Expected, Actual).

-endif.
