

-module(tutils).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

-export([aql/1,
          create_single_table/1,
          create_fk_table/2, create_fk_table/3,
          delete_by_key/2,
          read_keys/3,
          print_state/2]).

-export([assertState/3,
          assertExists/1,
          assert_table_policy/2]).

aql(Aql) ->
  aqlparser:parse({str, Aql}).

create_single_table(Name) ->
  Query = ["CREATE @AW TABLE ", Name, " (ID INT PRIMARY KEY)"],
  aql(lists:concat(Query)).

create_fk_table(Name, Pointer) ->
  create_fk_table(Name, Pointer, "ID").

create_fk_table(Name, TPointer, CPointer) ->
  Query = ["CREATE @AW TABLE ", Name,
    " (ID INT PRIMARY KEY, ", TPointer, " INT FOREIGN KEY @FR REFERENCES ",
    TPointer, "(", CPointer, "))"],
  aql(lists:concat(Query)).

delete_by_key(TName, Key) ->
  Query = ["DELETE FROM ", TName, " WHERE ID = ", Key],
  aql(lists:concat(Query)).

assertState(State, TName, Key) ->
  AQLKey = element:create_key(Key, TName),
  {ok, TxId} = antidote:start_transaction(),
  Table = table:lookup(TName, TxId),
  {ok, [Res]} = antidote:read_objects(AQLKey, TxId),
  Actual = element:is_visible(Res, Table, TxId),
  antidote:commit_transaction(TxId),
  ?assertEqual(State, Actual).

print_state(TName, Key) ->
  TNameAtom = utils:to_atom(TName),
  AQLKey = element:create_key(Key, TNameAtom),
  {ok, TxId} = antidote:start_transaction(),
  Table = table:lookup(TNameAtom, TxId),
  {ok, [Data]} = antidote:read_objects(AQLKey, TxId),
  io:fwrite("Tags for ~p(~p)~nData: ~p~n", [TNameAtom, Key, Data]),
  lists:foreach(fun(?T_FK(FkName, FkType, _, _)) ->
    FkValue = element:get(foreign_keys:to_cname(FkName), types:to_crdt(FkType), Data, Table),
    Tag = index:tag_read(TNameAtom, FkName, FkValue, TxId),
    io:fwrite("Tag(~p): ~p -> ~p~n", [FkValue, index:tag_name(TNameAtom, FkName), Tag])
  end, table:shadow_columns(Table)),
  io:fwrite("Final: ~p~n", [element:is_visible(Data, Table, TxId)]),
antidote:commit_transaction(TxId).

assert_table_policy(Expected, TName) ->
  TNameAtom = utils:to_atom(TName),
  {ok, TxId} = antidote:start_transaction(),
  Table = table:lookup(TNameAtom, TxId),
  antidote:commit_transaction(TxId),
  ?assertEqual(Expected, table:policy(Table)).

assertExists(Key) ->
  {ok, [Res], _CT} = antidote:read_objects(Key),
  ?assertNotEqual([], Res).

read_keys(Table, ID, Keys) ->
  Join = join_keys(Keys, []),
  Query = ["SELECT ", Join, " FROM ", Table, " WHERE ID = ", ID],
  {ok, [Res]} = aql(lists:concat(Query)),
  lists:map(fun({_k, V}) -> V end, Res).

join_keys([Key | Keys], []) ->
  join_keys(Keys, Key);
join_keys([Key | Keys], Acc) ->
  NewAcc = lists:concat([Acc, ", ", Key]),
  join_keys(Keys, NewAcc);
join_keys([], Acc) -> Acc.
