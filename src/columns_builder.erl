
-module(columns_builder).

-export([new/0,
        put_raw/2,
        build/1]).

new() ->
  {maps:new(), [], []}.

put_raw(RawCol, {Maps, Names, Pks}) ->
  Name = column:unwrap_name(RawCol),
  Type = column:unwrap_type(RawCol),
  Constraint = column:unwrap_constraint(RawCol),
  Col = ?T_COL(Name, Type, Constraint),
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
