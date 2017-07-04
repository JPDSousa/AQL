

-module(tutils).

-include_lib("eunit/include/eunit.hrl").

-export([aql/1,
          create_single_table/1,
          create_fk_table/2,
          delete_by_key/2]).

-export([assertState/3]).

aql(Aql) ->
  aqlparser:parse({str, Aql}).

create_single_table(Name) ->
  Query = ["CREATE TABLE ", Name, " (ID INT PRIMARY KEY)"],
  aql(lists:concat(Query)).

create_fk_table(Name, Pointer) ->
  Query = ["CREATE TABLE ", Name,
    " (ID INT PRIMARY KEY, ", Pointer, " INT FOREIGN KEY REFERENCES ",
    Pointer, "(ID))"],
  aql(lists:concat(Query)).

delete_by_key(TName, Key) ->
  Query = ["DELETE FROM ", TName, " WHERE ID = ", Key],
  aql(lists:concat(Query)).

assertState(State, TName, Key) ->
  Query = ["SELECT * FROM ", TName, " WHERE ID = ", Key],
  {ok, [Res]} = tutils:aql(lists:concat(Query)),
  ?assertEqual(State, element:st_value(Res)).
