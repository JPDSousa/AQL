

-module(tutils).

-export([aql/1]).

aql(Aql) ->
  aqlparser:parse({str, Aql}).
