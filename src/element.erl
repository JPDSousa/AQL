
-module(element).

-define(DATA_ENTRY(Key, Crdt), {Key, Crdt}).
-define(CRDT_TYPE, antidote_crdt_gmap).
-define(EL_KEY, '#key').
-define(EL_COLS, '#cols').
-define(EL_PK, '#pk').
-define(EL_ST, ?DATA_ENTRY('#st', antidote_crdt_mvreg)).
-define(EL_REFS, ?DATA_ENTRY('#refs', antidote_crdt_gset)).
-define(EL_ANON, none).

-include("aql.hrl").
-include("parser.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/1, new/2, create_key/2,
        put/3,
        get/3,
        create_db_op/1,
        primary_key/1,
        attributes/1]).

%% ====================================================================
%% API functions
%% ====================================================================

create_key(Key, TName) ->
  crdt:create_bound_object(Key, ?CRDT_TYPE, TName).

primary_key_column(Element) ->
  dict:fetch(?EL_KEY, Element).

new(Table) when ?is_table(Table) ->
  new(?EL_ANON, Table).

new(Key, Table) when ?is_dbkey(Key) and ?is_table(Table) ->
  Bucket = table:name(Table),
  BoundObject = create_key(Key, Bucket),
  Columns = table:get_columns(Table),
  PrimaryKey = table:primary_key(Table),
  El0 = dict:new(),
  El1 = dict:store(?EL_KEY, BoundObject, El0),
  El2 = dict:store(?EL_COLS, Columns, El1),
  El3 = dict:store(?EL_PK, PrimaryKey, El2),
  El4 = dict:store(?EL_ST, ipa:new(), El3),
  load_defaults(dict:to_list(Columns), El4).

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
  Res = dict:find(ColName, attributes(Element)),
  case Res of
    {ok, Col} ->
      ColType = column:type(Col),
      Element1 = set_if_primary(Col, Value, Element),
      Element2 = append(ColName, Value, ColType, Element1),
      {ok, Element2};
    _Else ->
      throwNoSuchColumn(ColName)
  end.

set_if_primary(Col, Value, Element) ->
  case column:is_primarykey(Col) of
    true ->
      set_key(Value, Element);
    _Else ->
      Element
  end.

get(ColName, Crdt, Element) when ?is_cname(ColName) ->
  Res = dict:find(?DATA_ENTRY(ColName, Crdt), Element),
  case Res of
    {ok, Value} ->
      Value;
    _Else ->
      throwNoSuchColumn(ColName)
  end.

create_db_op(Element) ->
  DataMap = dict:filter(fun is_data_field/2, Element),
  Ops = dict:to_list(DataMap),
  Key = primary_key_column(Element),
  crdt:create_map_update(Key, Ops).

is_data_field(?EL_KEY, _V) -> false;
is_data_field(?EL_PK, _V) -> false;
is_data_field(?EL_COLS, _V) -> false;
is_data_field(_Key, _V) -> true.

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
  dict:fetch(?EL_COLS, Element).

primary_key(Element) ->
  dict:fetch(?EL_PK, Element).

throwInvalidType(Type, CollumnName) ->
	throw(lists:concat(["Invalid type ", Type, " for collumn: ", CollumnName])).

throwNoSuchColumn(ColName) ->
  throw(lists:concat(["Column ", ColName, " does not exist."])).

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
  ?assertEqual(Expected, create_key(Key, TName)).

new_test() ->
  Key = key,
  Table = create_table_aux(),
  BoundObject = create_key(Key, table:name(Table)),
  Columns = table:get_columns(Table),
  Pk = table:primary_key(Table),
  Data = dict:to_list(load_defaults(dict:to_list(Columns), dict:new())),
  Element = new(Key, Table),
  ?assertEqual(5, dict:size(Element)),
  ?assertEqual(BoundObject, dict:fetch(?EL_KEY, Element)),
  ?assertEqual(Columns, dict:fetch(?EL_COLS, Element)),
  ?assertEqual(Pk, dict:fetch(?EL_PK, Element)),
  AssertPred = fun ({K, V}) -> ?assertEqual(V, dict:fetch(K, Element)) end,
  lists:foreach(AssertPred, Data).

new_1_test() ->
  Table = create_table_aux(),
  ?assertEqual(new(?EL_ANON, Table), new(Table)).

append_raw_test() ->
  Table = create_table_aux(),
  Value = ?PARSER_STRING("Value"),
  Op = fun crdt:assign_lww/1,
  Element = new(key, Table),
  % assert not fail
  append(key, Value, ?CRDT_VARCHAR, ?PARSER_STRING_TOKEN, Op, Element).

put_test() ->
  Table = create_table_aux(),
  El = new('1', Table),
  {ok, El1} = put([?PARSER_ATOM('NationalRank')], [?PARSER_NUMBER(3)], El),
  ?assertEqual(crdt:set_integer(3), get('NationalRank', ?CRDT_INTEGER, El1)).

get_default_test() ->
  Key = key,
  Table = create_table_aux(),
  El = new(Key, Table),
  ?assertEqual(crdt:set_integer(1), get('NationalRank', ?CRDT_INTEGER, El)).

-endif.
