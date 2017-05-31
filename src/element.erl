
-module(element).

-define(CRDT_TYPE, antidote_crdt_gmap).
-define(EL_KEY, key).
-define(EL_COLS, columns).
-define(EL_PK, primary_key).
-define(EL_DATA, data).
-define(EL_ANON, none).
-define(DATA_ENTRY(Key, Crdt), {Key, Crdt}).

-include("aql.hrl").
-include("parser.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, new/2, key/2,
        put/3,
        update/3,
        get/3,
        create_db_op/1,
        primary_key/1,
        attributes/1]).

%% ====================================================================
%% API functions
%% ====================================================================

key(Key, TName) ->
  crdt:create_bound_object(Key, ?CRDT_TYPE, TName).

new(Table) when ?is_table(Table) ->
  new(?EL_ANON, Table).

new(Key, Table) when ?is_dbkey(Key) and ?is_table(Table) ->
  Bucket = table:name(Table),
  BoundObject = key(Key, Bucket),
  Columns = table:get_columns(Table),
  PrimaryKey = table:primary_key(Table),
  El0 = dict:new(),
  El1 = dict:store(?EL_KEY, BoundObject, El0),
  El2 = dict:store(?EL_COLS, Columns, El1),
  El3 = dict:store(?EL_PK, PrimaryKey, El2),
  dict:store(?EL_DATA, load_defaults(Columns), El3).

load_defaults(_Columns) ->
  dict:new().

update(Key, [Op | Ops], Element) when tuple_size(Key) =:= 2 and ?is_element(Element) ->
  Element1 = update(Key, Op, Element),
  update(Key, Ops, Element1);
update(Key, [], Element) when tuple_size(Key) =:= 2 and ?is_element(Element) ->
  Element;
update({Key, Type}, Op, Element) when ?is_element(Element) ->
  Cols = attributes(Element),
  case maps:find(Key, Cols) of
    {ok, _Value} ->
      append({Key, Type}, Op, Element);
    _Else ->
      {err, badkey}
  end.

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
      Element1 = set_if_primary(Col, Value, Element),
      Element2 = append(ColName, Value, ColType, Element1),
      {ok, Element2}
  end.

set_if_primary(Col, Value, Element) ->
  case column:is_primarykey(Col) of
    true ->
      set_key(Value, Element);
    _Else ->
      Element
  end.

get(ColName, Crdt, Element) when ?is_cname(ColName) and ?is_element(Element)->
  Data = maps:get(?EL_DATA, Element),
  maps:get(?DATA_ENTRY(ColName, Crdt), Data).

create_db_op(Element) when ?is_element(Element) ->
  DataMap = maps:get(?EL_DATA, Element),
  Ops = maps:to_list(DataMap),
  Key = key(Element),
  crdt:create_map_update(Key, Ops).

key(Element) when ?is_element(Element) ->
  maps:get(?EL_KEY, Element).

set_key(?PARSER_TYPE(_Type, Value), Element) when ?is_element(Element) ->
  {_Key, Type, Bucket} = maps:get(?EL_KEY, Element),
  Element#{?EL_KEY => crdt:create_bound_object(Value, Type, Bucket)}.

append(Key, WrappedValue, ?AQL_INTEGER, Element) ->
  case WrappedValue of
    ?PARSER_NUMBER(Value) ->
      Op = crdt:set_integer(Value),
      append(?DATA_ENTRY(Key, ?CRDT_INTEGER), Op, Element);
    {Type, _Value} ->
      throwInvalidType(Type, Key)
  end;
append(Key, WrappedValue, ?AQL_VARCHAR, Element) ->
  case WrappedValue of
    ?PARSER_STRING(Value) ->
      Op = crdt:assign_lww(Value),
      append(?DATA_ENTRY(Key, ?CRDT_VARCHAR), Op, Element);
    {Type, _Value} ->
      throwInvalidType(Type, Key)
  end;
append(Key, WrappedValue, ?AQL_COUNTER_INT, Element) ->
  case WrappedValue of
    ?PARSER_NUMBER(Value) ->
      Op = crdt:increment_counter(Value),
      append(?DATA_ENTRY(Key, ?CRDT_COUNTER_INT), Op, Element);
    {Type, _Value} ->
      throwInvalidType(Type, Key)
  end.

append(Key, Value, Element) when ?is_element(Element)->
  Data0 = maps:get(?EL_DATA, Element),
  Data1 = Data0#{Key => Value},
  Element#{?EL_DATA => Data1}.

attributes(Element) when ?is_element(Element) ->
  maps:get(?EL_COLS, Element).

primary_key(Element) when ?is_element(Element)->
  maps:get(?EL_PK, Element).

throwInvalidType(Type, CollumnName) ->
	{err, lists:concat(["Invalid type ", Type, " for collumn: ", CollumnName])}.

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

key_test() ->
  Key = key,
  TName = test,
  Expected = crdt:create_bound_object(Key, ?CRDT_TYPE, TName),
  ?assertEqual(Expected, key(Key, TName)).

new_test() ->
  Key = key,
  {ok, Tokens, _} = scanner:string("CREATE LWW TABLE Universities (WorldRank INT PRIMARY KEY , Institution VARCHAR , NationalRank VARCHAR);"),
	{ok, [{?CREATE_TOKEN, Table}]} = parser:parse(Tokens),
  BoundObject = key(Key, table:name(Table)),
  Columns = table:get_columns(Table),
  Pk = table:primary_key(Table),
  Data = load_defaults(Columns),
  Element = new(Key, Table),
  ?assertEqual(4, dict:size(Element)),
  ?assertEqual(BoundObject, dict:fetch(?EL_KEY, Element)),
  ?assertEqual(Columns, dict:fetch(?EL_COLS, Element)),
  ?assertEqual(Pk, dict:fetch(?EL_PK, Element)),
  ?assertEqual(Data, dict:fetch(?EL_DATA, Element)).
-endif.
