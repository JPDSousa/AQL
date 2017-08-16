
-module(element).

-include("aql.hrl").
-include("parser.hrl").
-include("types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CRDT_TYPE, antidote_crdt_gmap).
-define(EL_ANON, none).

-export([primary_key/1, set_primary_key/2,
        foreign_keys/1, foreign_keys/2, foreign_keys/3,
        attributes/1,
        data/1,
        table/1]).

-export([create_key/2, st_key/0,
        is_visible/2, is_visible/3]).

-export([new/1, new/2,
        put/3, build_fks/2,
        get/2, get/3, get/4,
        get_by_name/2,
        insert/1, insert/2]).

%% ====================================================================
%% Property functions
%% ====================================================================

ops({_BObj, _Table, Ops, _Data}) -> Ops.
set_ops({BObj, Table, _Ops, Data}, Ops) -> ?T_ELEMENT(BObj, Table, Ops, Data).

primary_key({BObj, _Table, _Ops, _Data}) -> BObj.
set_primary_key({_BObj, Table, Ops, Data}, BObj) -> ?T_ELEMENT(BObj, Table, Ops, Data).

foreign_keys(Element) ->
  foreign_keys:from_columns(attributes(Element)).

attributes(Element) ->
  Table = table(Element),
  table:columns(Table).

data({_BObj, _Table, _Ops, Data}) -> Data.
set_data({BObj, Table, Ops, _Data}, Data) -> ?T_ELEMENT(BObj, Table, Ops, Data).

table({_BObj, Table, _Ops, _Data}) -> Table.

%% ====================================================================
%% Utils functions
%% ====================================================================

create_key(Key, TName) ->
  KeyAtom = utils:to_atom(Key),
  crdt:create_bound_object(KeyAtom, ?CRDT_TYPE, TName).

st_key() ->
  ?MAP_KEY('#st', antidote_crdt_mvreg).

explicit_state(Element) when is_tuple(Element) ->
  explicit_state(data(Element));
explicit_state(Data) ->
  Value = proplists:get_value(st_key(), Data),
  case Value of
    undefined ->
      throw("No explicit state found");
    _Else ->
      ipa:status(ipa:add_wins(), Value)
  end.

is_visible(Element, TxId) when is_tuple(Element) ->
  Data = data(Element),
  Table = table(Element),
  is_visible(Data, Table, TxId).

is_visible([], _Table, _TxId) -> false;
is_visible(Data, Table, TxId) ->
  TName = table:name(Table),
  ExplicitState = explicit_state(Data),
	Fks = table:shadow_columns(Table),
  ImplicitState = lists:map(fun(?T_FK(FkName, FkType, _, _)) ->
    FkValue = element:get(foreign_keys:to_cname(FkName), types:to_crdt(FkType), Data, Table),
    FkState = index:tag_read(TName, FkName, FkValue, TxId),
    ipa:status(ipa:add_wins(), FkState)
  end, Fks),
  ipa:is_visible(ExplicitState, ImplicitState).

throwInvalidType(Type, ColumnName, TableName) ->
	throw(lists:concat(["Invalid type ", Type, " for collumn: ",
  ColumnName, " in table ", TableName])).

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
  StateOp = crdt:field_map_op(st_key(), crdt:assign_lww(ipa:new())),
  Ops = [StateOp],
  Element = ?T_ELEMENT(BoundObject, Table, Ops, []),
  load_defaults(Element).

load_defaults(Element) ->
  Columns = attributes(Element),
  Defaults = column:s_filter_defaults(Columns),
  maps:fold(fun (CName, Column, Acc) ->
    {?DEFAULT_TOKEN, Value} = column:constraint(Column),
    append(CName, Value, column:type(Column), Acc)
  end, Element, Defaults).

put([Key | OKeys], [Value | OValues], Element) ->
  utils:assert_same_size(OKeys, OValues, "Illegal number of keys and values"),
  Res = put(Key, Value, Element),
  put(OKeys, OValues, Res);
put([], [], Element) ->
  {ok, Element};
put(ColName, Value, Element) ->
  ColSearch = maps:get(ColName, attributes(Element)),
  case ColSearch of
    {badkey, _} ->
      Table = table(Element),
      TName = table:name(Table),
      throwNoSuchColumn(ColName, TName);
    Col ->
      ColType = column:type(Col),
      Element1 = set_if_primary(Col, Value, Element),
      append(ColName, Value, ColType, Element1)
  end.

set_if_primary(Col, Value, Element) ->
  case column:is_primary_key(Col) of
    true ->
      ?BOUND_OBJECT(_Key, _Type, Bucket) = primary_key(Element),
      set_primary_key(Element, create_key(Value, Bucket));
    _Else ->
      Element
  end.

build_fks(Element, TxId) ->
  Data = data(Element),
  Table = table(Element),
  Fks = table:shadow_columns(Table),
  Parents = parents(Data, Fks, Table, TxId),
  lists:foldl(fun(?T_FK(FkName, FkType, _, _), AccElement) ->
    case length(FkName) of
      1 -> AccElement;
      _Else ->
        [{_, ParentId} | ParentCol] = FkName,
        Parent = dict:fetch(ParentId, Parents),
        Value = get_by_name(foreign_keys:to_cname(ParentCol), Parent),
        append(FkName, Value, FkType, AccElement)
    end
  end, Element, Fks).

parents(Data, Fks, Table, TxId) ->
  lists:foldl(fun(?T_FK(Name, Type, TTName, _), Dict) ->
    case Name of
      [ShCol] ->
        {_FkTable, FkName} = ShCol,
        Value = get(FkName, types:to_crdt(Type), Data, Table),
        Key = create_key(Value, TTName),
        {ok, [Parent]} = antidote:read_objects(Key, TxId),
        dict:store(FkName, Parent, Dict);
      _Else -> Dict
    end
  end, dict:new(), Fks).


get_by_name(ColName, [{{ColName, _Type}, Value} | _]) ->
	Value;
get_by_name(ColName, [_KV | Data]) ->
	get_by_name(ColName, Data);
get_by_name(_ColName, []) -> undefined.

get(ColName, Element) ->
  Columns = attributes(Element),
  Col = maps:get(ColName, Columns),
  AQL = column:type(Col),
  get(ColName, types:to_crdt(AQL), Element).

get(ColName, Crdt, Element) when ?is_element(Element) ->
  get(ColName, Crdt, data(Element), table(Element)).

get(ColName, Crdt, Data, Table) when is_atom(Crdt) ->
  Value = proplists:get_value(?MAP_KEY(ColName, Crdt), Data),
  case Value of
    undefined ->
      TName = table:name(Table),
      throwNoSuchColumn(ColName, TName);
    _Else ->
      Value
    end;
get(ColName, Cols, Data, TName) ->
  Col = maps:get(ColName, Cols),
  AQL = column:type(Col),
  get(ColName, types:to_crdt(AQL), Data, TName).

insert(Element) ->
  Ops = ops(Element),
  Key = primary_key(Element),
  crdt:map_update(Key, Ops).
insert(Element, TxId) ->
  Op = insert(Element),
  antidote:update_objects(Op, TxId).

append(Key, Value, AQL, Element) ->
  Data = data(Element),
  Ops = ops(Element),
  OffValue = apply_offset(Key, Value, Element),
  OpKey = ?MAP_KEY(Key, types:to_crdt(AQL)),
  OpVal = types:to_insert_op(AQL, OffValue),
  Element1 = set_data(Element, lists:append(Data, [{OpKey, Value}])),
  set_ops(Element1, lists:append(Ops, [{OpKey, OpVal}])).


apply_offset(Key, Value, Element) when is_atom(Key) ->
  Col = maps:get(Key, attributes(Element)),
  Type = column:type(Col),
  Cons = column:constraint(Col),
  case {Type, Cons} of
    {?AQL_COUNTER_INT, ?CHECK_KEY({?COMPARATOR_KEY(Comp), Offset})} ->
      bcounter:to_bcounter(Key, Value, Offset, Comp);
    _Else -> Value
  end;
apply_offset(_Key, Value, _Element) -> Value.

foreign_keys(Fks, Element) when is_tuple(Element) ->
  Data = data(Element),
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

primary_key_test() ->
  Table = eutils:create_table_aux(),
  Element = new(key, Table),
  ?assertEqual(create_key(key, 'Universities'), primary_key(Element)).

attributes_test() ->
  Table = eutils:create_table_aux(),
  Columns = table:columns(Table),
  Element = new(key, Table),
  ?assertEqual(Columns, attributes(Element)).

create_key_test() ->
  Key = key,
  TName = test,
  Expected = crdt:create_bound_object(Key, ?CRDT_TYPE, TName),
  ?assertEqual(Expected, create_key(Key, TName)).

new_test() ->
  Key = key,
  Table = eutils:create_table_aux(),
  BoundObject = create_key(Key, table:name(Table)),
  Ops = [crdt:field_map_op(st_key(), crdt:assign_lww(ipa:new()))],
  Expected = ?T_ELEMENT(BoundObject, Table, Ops, []),
  Expected1 = load_defaults(Expected),
  Element = new(Key, Table),
  ?assertEqual(Expected1, Element),
  ?assertEqual(crdt:assign_lww(ipa:new()), proplists:get_value(st_key(), ops(Element))).

new_1_test() ->
  Table = eutils:create_table_aux(),
  ?assertEqual(new(?EL_ANON, Table), new(Table)).

append_raw_test() ->
  Table = eutils:create_table_aux(),
  Value = 9,
  Element = new(key, Table),
  % assert not fail
  append('NationalRank', Value, ?AQL_INTEGER, Element).

get_default_test() ->
  Table = eutils:create_table_aux(),
  El = new(key, Table),
  ?assertEqual("aaa", get('InstitutionId', ?CRDT_VARCHAR, El)).

get_by_name_test() ->
  Data = [{{a, abc}, 1}, {{b, abc}, 2}],
  ?assertEqual(1, get_by_name(a, Data)),
  ?assertEqual(undefined, get_by_name(c, Data)),
  ?assertEqual(undefined, get_by_name(a, [])).

-endif.
