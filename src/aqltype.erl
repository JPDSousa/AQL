
-module(aqltype).

-include("aql.hrl").

-export([aql_to_crdt/1]).

aql_to_crdt(Aql) ->
  case Aql of
    ?AQL_INTEGER ->
      ?CRDT_INTEGER;
    ?AQL_VARCHAR ->
      ?CRDT_VARCHAR;
    ?AQL_BOOLEAN ->
      ?CRDT_BOOLEAN;
    ?AQL_COUNTER_INT ->
      ?CRDT_COUNTER_INT
  end.
