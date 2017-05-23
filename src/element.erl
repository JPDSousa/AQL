
-module(element).

-define(CRDT_TYPE, antidote_crdt_gmap).
-define(EL_KEY, key).
-define(EL_COLS, columns).
-define(EL_PK, primary_key).
-define(EL_DATA, data).

-include("aql.hrl").
-include("parser.hrl").

-export([new/2,
        put/3,
        put_op/3,
        create_db_op/1,
        primary_key/1,
        attributes/1]).

%% ====================================================================
%% API functions
%% ====================================================================

new(Key, Table) when ?is_dbkey(Key) and ?is_table(Table) ->
  Bucket = table:name(Table),
  BoundObject = crdt:create_bound_object(Key, ?CRDT_TYPE, Bucket),
  Columns = table:get_columns(Table),
  PrimaryKey = table:primary_key(Columns),
  Map0 = #{?EL_KEY => BoundObject, ?EL_COLS => Columns, ?EL_PK => PrimaryKey},
  Map0#{?EL_DATA => #{}}.

put_op({Key, CRDT_TYPE}, Op, Element) when ?is_element(Element) ->
  append({Key, CRDT_TYPE}, Op, Element).

put([Key, OKeys], [Value, OValues], Element) when ?is_element(Element) and length(OKeys) =:= length(OValues) ->
  %check if Keys and Values have the same size
  Res = put(Key, Value, Element),
  case Res of
    {ok, NewElement} ->
      put(OKeys, OValues, NewElement);
    {err, _Msg} ->
      Res
  end;
put([], [], Element) ->
  {ok, Element};
put(ColName, Value, Element) when ?is_cname(ColName) and ?is_element(Element) ->
  Col = maps:get(ColName, attributes(Element)),
  case Col of
    {badkey, _Key} ->
      {err, lists:concat(["Column ", ColName, " does not exist."])};
    _Else ->
      ColType = column:type(Col),
      NewElement = append(ColName, Value, ColType, Element),
      {ok, NewElement}
  end.

create_db_op(Element) when ?is_element(Element) ->
  DataMap = maps:get(?EL_DATA, Element),
  maps:to_list(DataMap).

append(Key, WrappedValue, ?AQL_INTEGER, Element) ->
  case WrappedValue of
    ?PARSER_NUMBER(Value) ->
      Op = crdt:set_integer(Value),
      append({Key, ?CRDT_INTEGER}, Op, Element);
    {Type, _Value} ->
      throwInvalidType(Type, Key)
  end;
append(Key, WrappedValue, ?AQL_VARCHAR, Element) ->
  case WrappedValue of
    ?PARSER_STRING(Value) ->
      Op = crdt:assign_lww(Value),
      append({Key, ?CRDT_VARCHAR}, Op, Element);
    {Type, _Value} ->
      throwInvalidType(Type, Key)
  end;
append(Key, WrappedValue, ?AQL_COUNTER_INT, Element) ->
  case WrappedValue of
    ?PARSER_NUMBER(Value) ->
      Op = crdt:increment_counter(Value),
      append({Key, ?CRDT_COUNTER_INT}, Op, Element);
    {Type, _Value} ->
      throwInvalidType(Type, Key)
  end.

append(Key, Value, Element) ->
  Data0 = maps:get(?EL_DATA, Element),
  Data1 = Data0#{Key => Value},
  Element#{?EL_DATA => Data1}.

attributes(Element) when ?is_element(Element) ->
  maps:get(?EL_COLS, Element).

primary_key(Element) when ?is_element(Element)->
  maps:get(?EL_PK, Element).

throwInvalidType(Type, CollumnName) ->
	{err, lists:concat(["Invalid type ", Type, " for collumn: ", CollumnName])}.
