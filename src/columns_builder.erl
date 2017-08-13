
-module(columns_builder).

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
