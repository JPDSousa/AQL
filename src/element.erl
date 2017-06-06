
-module(element).

-define(CRDT_TYPE, antidote_crdt_gmap).
-define(EL_KEY, '#key').
-define(EL_COLS, '#cols').
-define(EL_PK, '#pk').
-define(EL_ANON, none).
-define(DATA_ENTRY(Key, Crdt), {Key, Crdt}).

-include("aql.hrl").
-include("parser.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, new/2, key/2,
        put/3,
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
  load_defaults(dict:to_list(Columns), El3).

load_defaults([{CName, Column}|Columns], Element) ->
  Constraint = column:constraint(Column),
  case Constraint of
    {?DEFAULT_TOKEN, Value} ->
      NewEl = append(CName, Value, column:type(Column), Element),
      load_defaults(Columns, NewEl);
    _Else ->
      load_defaults(Columns, Element)
  end;
load_defaults([], Element) ->
  Element.

put([Key | OKeys], [Value | OValues], Element) ->
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
put(?PARSER_ATOM(ColName), Value, Element) when ?is_cname(ColName) ->
  Col = dict:fetch(ColName, attributes(Element)),
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

get(ColName, Crdt, Element) when ?is_cname(ColName) ->
  dict:fetch(?DATA_ENTRY(ColName, Crdt), Element).

create_db_op(Element) ->
  DataMap = dict:filter(fun is_data_field/2, Element),
  Ops = maps:to_list(DataMap),
  Key = key(Element),
  crdt:create_map_update(Key, Ops).

is_data_field(?EL_KEY, _V) -> false;
is_data_field(?EL_PK, _V) -> false;
is_data_field(?EL_COLS, _V) -> false;
is_data_field(_Key, _V) -> false.

key(Element) ->
  maps:get(?EL_KEY, Element).

set_key(?PARSER_TYPE(_Type, Value), Element) ->
  {_Key, Type, Bucket} = dict:fetch(?EL_KEY, Element),
  dict:store(?EL_KEY, crdt:create_bound_object(Value, Type, Bucket), Element).

append(Key, WrappedValue, ?AQL_INTEGER, Element) ->
  append(Key, WrappedValue, ?CRDT_INTEGER, ?PARSER_NUMBER_TOKEN, fun crdt:set_integer/1, Element);
append(Key, WrappedValue, ?AQL_VARCHAR, Element) ->
  append(Key, WrappedValue, ?CRDT_VARCHAR, ?PARSER_STRING_TOKEN, fun crdt:assign_lww/1, Element);
append(Key, WrappedValue, ?AQL_COUNTER_INT, Element) ->
  append(Key, WrappedValue, ?CRDT_COUNTER_INT, ?PARSER_NUMBER_TOKEN, fun crdt:increment_counter/1, Element).

append(Key, WrappedValue, Crdt, PTToken, Op, Element) ->
  case WrappedValue of
    ?PARSER_TYPE(PTToken, Value) ->
      OpVal = Op(Value),
      dict:store(?DATA_ENTRY(Key, Crdt), OpVal, Element);
    {Type, _Value} ->
      throwInvalidType(Type, Key)
  end.

attributes(Element) ->
  maps:get(?EL_COLS, Element).

primary_key(Element) ->
  maps:get(?EL_PK, Element).

throwInvalidType(Type, CollumnName) ->
	{err, lists:concat(["Invalid type ", Type, " for collumn: ", CollumnName])}.

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

create_table_aux() ->
  {ok, Tokens, _} = scanner:string("CREATE LWW TABLE Universities (WorldRank INT PRIMARY KEY , Institution VARCHAR , NationalRank INTEGER DEFAULT 1);"),
	{ok, [{?CREATE_TOKEN, Table}]} = parser:parse(Tokens),
  Table.

key_test() ->
  Key = key,
  TName = test,
  Expected = crdt:create_bound_object(Key, ?CRDT_TYPE, TName),
  ?assertEqual(Expected, key(Key, TName)).

new_test() ->
  Key = key,
  Table = create_table_aux(),
  BoundObject = key(Key, table:name(Table)),
  Columns = table:get_columns(Table),
  Pk = table:primary_key(Table),
  Data = dict:to_list(load_defaults(dict:to_list(Columns), dict:new())),
  Element = new(Key, Table),
  ?assertEqual(4, dict:size(Element)),
  ?assertEqual(BoundObject, dict:fetch(?EL_KEY, Element)),
  ?assertEqual(Columns, dict:fetch(?EL_COLS, Element)),
  ?assertEqual(Pk, dict:fetch(?EL_PK, Element)),
  AssertPred = fun ({K, V}) -> ?assertEqual(V, dict:fetch(K, Element)) end,
  lists:foreach(AssertPred, Data).

new_1_test() ->
  Table = create_table_aux(),
  ?assertEqual(new(?EL_ANON, Table), new(Table)).

append_raw_test() ->
  Key = key,
  Table = create_table_aux(),
  % assert not fail
  append(key, ?PARSER_STRING("Value"), ?CRDT_VARCHAR, ?PARSER_STRING_TOKEN, fun crdt:assign_lww/1, new(Key, Table)),
  ?assertEqual(true, true).

create_db_op_test() ->
  Key = key,
  Table = create_table_aux(),
  El = new(Key, Table),
  io:fwrite("~p~n", [dict:to_list(El)]),
  ?assertEqual(crdt:set_integer(1), get('NationalRank', ?CRDT_INTEGER, El)).

-endif.
