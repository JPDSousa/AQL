%% @author JPDSousa
%% @doc module used to build column metadata

-module(columns_builder).

-include_lib("parser.hrl").
-include_lib("aql.hrl").
-include_lib("types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/0,
        put_raw/2,
        build/1]).

new() ->
  create(maps:new(), [], [], []).

create(Cols, Names, Pks, CRPs) ->
  {Cols, Names, Pks, CRPs}.

put_raw(Col, {Maps, Names, Pks, CRPs}) ->
  Name = column:name(Col),
  NewNames = lists:append(Names, [Name]),
  NewMaps = maps:put(Name, Col, Maps),
  handle_constraint(fun(NewPks) ->
    create(NewMaps, NewNames, NewPks, CRPs)
  end, fun(NewCol, Crp) ->
    NewMaps1 = maps:put(Name, NewCol, Maps),
    create(NewMaps1, NewNames, Pks, lists:append(CRPs, [{Name, Crp}]))
  end, fun() ->
    create(NewMaps, NewNames, Pks, CRPs)
  end, Col, Name, Pks).

handle_constraint(IsPk, IsFk, Else, Col, CName, Pks) ->
  case column:is_primary_key(Col) of
    true ->
      NewPks = lists:append(Pks, [CName]),
      IsPk(NewPks);
    _Else ->
      case column:is_foreign_key(Col) of
        true ->
          {NewCol, Crp} = remove_dep_crp(Col),
          IsFk(NewCol, Crp);
        _Else ->
          Else()
      end
  end.

remove_dep_crp(Col) ->
  ?FOREIGN_KEY({TName, CName, Crp}) = column:constraint(Col),
  {column:set_constraint(?FOREIGN_KEY({TName, CName}), Col), Crp}.

build({Maps, Names, Pks, CRPs}) ->
  Build = maps:put(?C_NAMES, Names, Maps),
  {maps:put(?C_PK, Pks, Build), CRPs}.

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

new_test() ->
  ?assertEqual({maps:new(), [], [], []}, new()).

put_raw_test() ->
  Col = ?T_COL("Test", ?AQL_INTEGER, ?NO_CONSTRAINT),
  Expected = {#{"Test" => Col}, ["Test"], [], []},
  ?assertEqual(Expected, put_raw(Col, new())).

put_raw_pk_test() ->
  Col = ?T_COL("Test", ?AQL_INTEGER, ?PRIMARY_TOKEN),
  Expected = {#{"Test" => Col}, ["Test"], ["Test"], []},
  ?assertEqual(Expected, put_raw(Col, new())).

build_test() ->
  Col1 = ?T_COL("Test1", ?AQL_INTEGER, ?PRIMARY_TOKEN),
  Col2 = ?T_COL("Test2", ?AQL_VARCHAR, ?NO_CONSTRAINT),
  Expected = #{"Test1" => Col1, "Test2" => Col2, ?C_NAMES => ["Test1", "Test2"], ?C_PK => ["Test1"]},
  Actual1 = new(),
  Actual2 = put_raw(Col1, Actual1),
  Actual3 = put_raw(Col2, Actual2),
  ?assertEqual({Expected, []}, build(Actual3)).

-endif.
