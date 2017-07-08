

-module(types).

-include("aql.hrl").

-export([to_aql/1,
        to_crdt/1]).

to_aql(?CRDT_INTEGER) -> ?AQL_INTEGER;
to_aql(?CRDT_BOOLEAN) -> ?AQL_BOOLEAN;
to_aql(?CRDT_COUNTER_INT) -> ?AQL_COUNTER_INT;
to_aql(?CRDT_VARCHAR) -> ?AQL_VARCHAR;
to_aql(Invalid) -> throw(lists:concat(["No mapping available for: ", Invalid])).

to_crdt(?AQL_INTEGER) -> ?CRDT_INTEGER;
to_crdt(?AQL_BOOLEAN) -> ?CRDT_BOOLEAN;
to_crdt(?AQL_COUNTER_INT) -> ?CRDT_COUNTER_INT;
to_crdt(?AQL_VARCHAR) -> ?CRDT_VARCHAR;
to_crdt(Invalid) -> throw(lists:concat(["No mapping available for: ", Invalid])).
