

-module(tutils).

-include_lib("eunit/include/eunit.hrl").

-export([aql/1,
          create_single_table/1,
          create_fk_table/2, create_fk_table/3,
          delete_by_key/2]).

-export([assertState/3]).

aql(Aql) ->
  aqlparser:parse({str, Aql}).

create_single_table(Name) ->
  Query = ["CREATE TABLE ", Name, " (ID INT PRIMARY KEY)"],
  aql(lists:concat(Query)).

create_fk_table(Name, Pointer) ->
  create_fk_table(Name, Pointer, "ID").

create_fk_table(Name, TPointer, CPointer) ->
  Query = ["CREATE TABLE ", Name,
    " (ID INT PRIMARY KEY, ", TPointer, " INT FOREIGN KEY REFERENCES ",
    TPointer, "(", CPointer, "))"],
  aql(lists:concat(Query)).


delete_by_key(TName, Key) ->
  Query = ["DELETE FROM ", TName, " WHERE ID = ", Key],
  aql(lists:concat(Query)).

assertState(State, TName, Key) ->
  Query = ["SELECT * FROM ", TName, " WHERE ID = ", Key],
  {ok, [Res]} = aql(lists:concat(Query)),
  ?assertEqual(State, element:st_value(Res)).
