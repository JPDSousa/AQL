
-module(columns_builder).

-include("aql.hrl").
-include("parser.hrl").
-include("types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0,
        put_raw/2,
        build/1]).

new() ->
  {maps:new(), [], []}.

put_raw(Col, {Maps, Names, Pks}) ->
  Name = column:name(RawCol),
  NewMaps = maps:put(Name, Col, Maps),
  NewNames = lists:append(Names, [Names]),
  case column:is_primarykey(Col) of
    true ->
      NewPks = lists:append(Pks, [Name]),
      {NewMaps, NewNames, NewPks};
    _Else ->
      {NewMaps, NewNames, Pks}
  end.

build({Maps, Names, Pks}) ->
  Build = maps:put(?C_NAMES, Names, Maps),
  maps:put(?C_PK, Pks, Build).

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

new_test() ->
  ?assertEqual({maps:new(), [], []}, new()).

put_raw_test() ->
  Col = ?T_COL("Test", ?AQL_INTEGER, ?NO_CONSTRAINT),
  Expected = {#{"Test" => Col}, ["Test"], []},
  ?assertEqual(Expected, put_raw(Col, new())).

put_raw_pk_test() ->
  Col = ?T_COL("Test", ?AQL_INTEGER, ?PRIMARY_TOKEN),
  Expected = {#{"Test" => Col}, ["Test"], ["Test"]},
  ?assertEqual(Expected, put_raw(Col, new())).

build_test() ->
  Col1 = ?T_COL("Test1", ?AQL_INTEGER, ?PRIMARY_TOKEN),
  Col2 = ?T_COL("Test2", ?AQL_VARCHAR, ?NO_CONSTRAINT),
  Expected = #{"Test1" => Col1, "Test2" => Col2, ?C_NAMES => ["Test1", "Test2"], ?C_PK => ["Test1"]},
  Actual1 = new(),
  Actual2 = put_raw(Col1, Actual1),
  Actual3 = put_raw(Col2, Actual2),
  ?assertEqual(Expected, build(Actual3)).

-endif.