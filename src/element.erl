
-module(element).

-define(CRDT_TYPE, antidote_crdt_gmap).
-define(EL_KEY, key).
-define(EL_COLS, columns).
-define(EL_PK, primary_key).
-define(EL_DATA, data).
-define(EL_ANON, none).

-include("aql.hrl").
-include("parser.hrl").

-export([new/1, new/2,
        put/3,
        put_op/3,
        create_db_op/1,
        primary_key/1,
        attributes/1]).

%% ====================================================================
%% API functions
%% ====================================================================

new(Table) when ?is_table(Table) ->
  new(?EL_ANON, Table).

new(Key, Table) when ?is_dbkey(Key) and ?is_table(Table) ->
  Bucket = table:name(Table),
  BoundObject = crdt:create_bound_object(Key, ?CRDT_TYPE, Bucket),
  Columns = table:get_columns(Table),
  PrimaryKey = table:primary_key(Table),
  Map0 = #{?EL_KEY => BoundObject, ?EL_COLS => Columns, ?EL_PK => PrimaryKey},
  Map0#{?EL_DATA => #{}}.

put_op({Key, CRDT_TYPE}, Op, Element) when ?is_element(Element) ->
  append({Key, CRDT_TYPE}, Op, Element).

put([Key | OKeys], [Value | OValues], Element) when ?is_element(Element) ->
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
put(?PARSER_ATOM(ColName), Value, Element) when ?is_cname(ColName) and ?is_element(Element) ->
  Col = maps:get(ColName, attributes(Element)),
  case Col of
    {badkey, _Key} ->
      {err, lists:concat(["Column ", ColName, " does not exist."])};
    _Else ->
      ColType = column:type(Col),
      NewElement = append(ColName, Value, ColType, Element),
      {ok, NewElement}
  end.

get(ColName, Element) when ?is_cname(ColName) and ?is_element(Element)->
  Data = maps:get(?EL_DATA, Element),
  maps:get(ColName, Data).

create_db_op(Element) when ?is_element(Element) ->
  DataMap = maps:get(?EL_DATA, Element),
  Ops = maps:to_list(DataMap),
  Key = key(Element),
  crdt:create_map_update(Key, Ops).

key(Element) when ?is_element(Element) ->
  {Key, Type, Bucket} = maps:get(?EL_KEY, Element),
  case Key of
    ?EL_ANON ->
      PkCol = primary_key(Element),
      PkName = column:name(PkCol),
      %check if value is valid
      PkKey = get(PkName),
      crdt:create_bound_object(PkKey, Type, Bucket);
    _Else ->
      crdt:create_bound_object(Key, Type, Bucket)
  end.


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
