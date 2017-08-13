
-module(element).

-include("aql.hrl").
-include("parser.hrl").
-include("types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CRDT_TYPE, antidote_crdt_gmap).
-define(EL_ANON, none).

-export([primary_key/1,
        foreign_keys/1, foreign_keys/2, foreign_keys/3,
        attributes/1,
        data/1,
        table/1]).

-export([create_key/2, st_key/0,
        is_visible/2, is_visible/3]).

-export([new/1, new/2,
        put/5,
        get/2, get/3, get/4,
        get_by_name/2,
        insert/1, insert/2]).

%% ====================================================================
%% Property functions
%% ====================================================================

el_create(BObj, Table, Ops, Data) -> ?T_ELEMENT(BObj, Table, Ops, Data).

el_get_key({BObj, _Table, _Ops, _Data}) -> BObj.
el_set_key({_BObj, Table, Ops, Data}, BObj) -> el_create(BObj, Table, Ops, Data).

el_get_table({_BObj, Table, _Ops, _Data}) -> Table.
el_set_table({BObj, _Table, Ops, Data}, Table) -> el_create(BObj, Table, Ops, Data).

el_get_ops({_BObj, _Table, Ops, _Data}) -> Ops.
el_set_ops({BObj, Table, _Ops, Data}, Ops) -> el_create(BObj, Table, Ops, Data).

el_get_data({_BObj, _Table, _Ops, Data}) -> Data.
el_set_data({BObj, Table, Ops, _Data}, Data) -> el_create(BObj, Table, Ops, Data).

primary_key(Element) ->
  el_get_key(Element).

foreign_keys(Element) ->
  foreign_keys:from_columns(attributes(Element)).

attributes(Element) ->
  Table = el_get_table(Element),
  table:columns(Table).

data(Element) ->
  el_get_data(Element).

table(Element) ->
  ?BOUND_OBJECT(_K, _T, TName) = primary_key(Element),
  TName.

%% ====================================================================
%% Utils functions
%% ====================================================================

create_key(Key, TName) ->
  KeyAtom = utils:to_atom(Key),
  crdt:create_bound_object(KeyAtom, ?CRDT_TYPE, TName).

st_key() ->
  ?MAP_KEY('#st', antidote_crdt_mvreg).

explicit_state(Element) when is_tuple(Element) ->
  explicit_state(el_get_data(Element));
explicit_state(Data) ->
  Value = proplists:get_value(st_key(), Data),
  case Value of
    undefined ->
      throw("No explicit state found");
    _Else ->
      ipa:status(ipa:add_wins(), Value)
  end.

is_visible(Element, TxId) when is_tuple(Element) ->
  Data = el_get_data(Element),
  Table = el_get_table(Element),
  TName = table(Element),
  is_visible(Data, Table, TxId).

is_visible(Data, Table, TxId) ->
  TName = table:name(Table),
  ExplicitState = explicit_state(Data),
	Fks = foreign_keys:all(Table),
  ImplicitState = lists:map(fun(?T_FK(FkName, FkType, _, _)) ->
    FkValue = element:get(FkName, types:to_crdt(FkType), Data, TName),
    FkState = index:tag_read(TName, FkName, FkValue, TxId),
    ipa:status(ipa:add_wins(), FkState)
  end, Fks),
  ipa:is_visible(ExplicitState, ImplicitState).

throwInvalidType(Type, CollumnName, TableName) ->
	throw(lists:concat(["Invalid type ", Type, " for collumn: ",
  CollumnName, " in table ", TableName])).

throwNoSuchColumn(ColName, TableName) ->
  throw(lists:concat(["Column ", ColName,
    " does not exist in table ", TableName])).

%% ====================================================================
%% API functions
%% ====================================================================

new(Table) when ?is_table(Table) ->
  new(?EL_ANON, Table).

new(Key, Table) ->
  Bucket = table:name(Table),
  BoundObject = create_key(Key, Bucket),
  Columns = column:s_from_table(Table),
  StateOp = crdt:field_map_op(st_key(), crdt:assign_lww(ipa:new())),
  Ops = [StateOp],
  Element = el_create(BoundObject, Columns, Ops, []),
  load_defaults(Columns, Element).

load_defaults(Columns, Element) ->
  Defaults = column:s_filter_defaults(Columns),
  dict:fold(fun (CName, Column, Acc) ->
    {?DEFAULT_TOKEN, Value} = column:constraint(Column),
    append(CName, Value, column:type(Column), Acc)
  end, Element, Defaults).

put([Key | OKeys], [Value | OValues], Element, Tables, TxId) ->
  utils:assert_same_size(OKeys, OValues, "Illegal number of keys and values"),
  Res = put(Key, Value, Element, Tables, TxId),
  put(OKeys, OValues, Res, Tables, TxId);
put([], [], Element, _Tables, _TxId) ->
  {ok, Element};
put(?PARSER_ATOM(ColName), Value, Element, Tables, TxId) ->
  ColSearch = dict:find(ColName, attributes(Element)),
  case ColSearch of
    {ok, Col} ->
      ColType = column:type(Col),
      Element1 = handle_fk(Col, Value, Element, Tables, TxId),
      Element2 = set_if_primary(Col, Value, Element1),
      append(ColName, Value, ColType, Element2);
    _Else ->
      TName = table(Element),
      throwNoSuchColumn(ColName, TName)
  end.

set_if_primary(Col, ?PARSER_TYPE(_PType, Value), Element) ->
  case column:is_primarykey(Col) of
    true ->
      % TODO use macro
      {_Key, _Type, Bucket} = el_get_key(Element),
      el_set_key(Element, create_key(Value, Bucket));
    _Else ->
      Element
  end.

handle_fk(Col, ?PARSER_TYPE(_Type, Value), Element, Tables, TxId) ->
  handle_fk(Col, Value, Element, Tables, TxId);
handle_fk(Col, Value, Element, Tables, TxId) ->
  case column:constraint(Col) of
    ?FOREIGN_KEY({?PARSER_ATOM(FkTable), ?PARSER_ATOM(FkAttr)}) ->
      IFks = foreign_keys:load_chain([{FkAttr, FkTable}], Value, Tables, TxId),
      lists:foldl(fun ({CName, CType, CValue}, AccElement) ->
        append({CName}, CValue, CType, AccElement)
      end, Element, IFks);
    _Else ->
      Element
  end.

get_by_name(ColName, [{{Key, _Type}, Value}]) when Key =:= ColName ->
	Value;
get_by_name(_ColName, []) -> undefined;
get_by_name(ColName, [_KV | Data]) ->
	get_by_name(ColName, Data).

get(ColName, Element) ->
  Cols = el_get_cols(Element),
  Col = dict:fetch(ColName, Cols),
  AQL = column:type(Col),
  get(ColName, types:to_crdt(AQL), Element).

get(ColName, Crdt, Element) when is_tuple(Element) ->
  get(ColName, Crdt, el_get_data(Element), table(Element)).

get(ColName, Crdt, Data, TName) when is_atom(Crdt) ->
  ColNameAtom = utils:to_atom(ColName),
  Value = proplists:get_value(?MAP_KEY(ColNameAtom, Crdt), Data),
  case Value of
    undefined ->
      throwNoSuchColumn(ColName, TName);
    _Else ->
      Value
    end;
get(ColName, Cols, Data, TName) ->
  Col = dict:fetch(ColName, Cols),
  AQL = column:type(Col),
  get(ColName, types:to_crdt(AQL), Data, TName).

insert(Element) ->
  Ops = el_get_ops(Element),
  Key = el_get_key(Element),
  crdt:map_update(Key, Ops).
insert(Element, TxId) ->
  Op = insert(Element),
  antidote:update_objects(Op, TxId).

append(Key, ?PARSER_TYPE(Type, Value), AQL, Element) ->
  Token = types:to_parser(AQL),
  case Type of
    Token ->
      append(Key, Value, AQL, Element);
    _Else ->
      TName = table(Element),
      throwInvalidType(Type, Key, TName)
  end;
append(Key, Value, AQL, Element) ->
  Data = el_get_data(Element),
  Ops = el_get_ops(Element),
  OffValue = apply_offset(Key, Value, Element),
  OpKey = ?MAP_KEY(Key, types:to_crdt(AQL)),
  OpVal = types:to_insert_op(AQL, OffValue),
  Element1 = el_set_data(Element, lists:append(Data, [{OpKey, Value}])),
  el_set_ops(Element1, lists:append(Ops, [{OpKey, OpVal}])).


apply_offset(Key, Value, Element) when is_atom(Key) ->
  Col = dict:fetch(Key, attributes(Element)),
  Type = column:type(Col),
  Cons = column:constraint(Col),
  case {Type, Cons} of
    {?AQL_COUNTER_INT, {?COMPARATOR_KEY(Comp), ?PARSER_NUMBER(Offset)}} ->
      bcounter:to_bcounter(Key, Value, Offset, Comp);
    _Else -> Value
  end;
apply_offset(_Key, Value, _Element) -> Value.

foreign_keys(Fks, Element) when is_tuple(Element) ->
  Data = el_get_data(Element),
  TName = table(Element),
  foreign_keys(Fks, Data, TName).

foreign_keys(Fks, Data, TName) ->
  lists:map(fun({{CName, CType}, {FkTable, FkAttr}}) ->
    Value = get(CName, types:to_crdt(CType), Data, TName),
    {{CName, CType}, {FkTable, FkAttr}, Value}
  end, Fks).
%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

create_table_aux() ->
  {ok, Tokens, _} = scanner:string("CREATE LWW TABLE Universities (WorldRank INT PRIMARY KEY, InstitutionId VARCHAR FOREIGN KEY REFERENCES Institution(id), NationalRank INTEGER DEFAULT 1);"),
	{ok, [{?CREATE_TOKEN, Table}]} = parser:parse(Tokens),
  Table.

primary_key_test() ->
  Table = create_table_aux(),
  Element = new(key, Table),
  ?assertEqual(create_key(key, 'Universities'), primary_key(Element)).

attributes_test() ->
  Table = create_table_aux(),
  Columns = column:s_from_table(Table),
  Element = new(key, Table),
  ?assertEqual(Columns, attributes(Element)).

create_key_test() ->
  Key = key,
  TName = test,
  Expected = crdt:create_bound_object(Key, ?CRDT_TYPE, TName),
  ?assertEqual(Expected, create_key(Key, TName)).

new_test() ->
  Key = key,
  Table = create_table_aux(),
  BoundObject = create_key(Key, table:name(Table)),
  Columns = column:s_from_table(Table),
  Ops = [crdt:field_map_op(st_key(), crdt:assign_lww(ipa:new()))],
  Expected = el_create(BoundObject, Columns, Ops, []),
  Expected1 = load_defaults(Columns, Expected),
  Element = new(Key, Table),
  ?assertEqual(Expected1, Element),
  ?assertEqual(crdt:assign_lww(ipa:new()), proplists:get_value(st_key(), el_get_ops(Element))).

new_1_test() ->
  Table = create_table_aux(),
  ?assertEqual(new(?EL_ANON, Table), new(Table)).

append_raw_test() ->
  Table = create_table_aux(),
  Value = ?PARSER_NUMBER(9),
  Element = new(key, Table),
  % assert not fail
  append('NationalRank', Value, ?AQL_INTEGER, Element).

get_default_test() ->
  Table = create_table_aux(),
  El = new(key, Table),
  ?assertEqual(1, get('NationalRank', ?CRDT_INTEGER, El)).

-endif.
