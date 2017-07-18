

-module(types).

-include("aql.hrl").
-include_lib("parser.hrl").

-export([to_aql/1,
        to_crdt/1,
        to_parser/1,
        to_insert_op/2]).

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

to_parser(?AQL_INTEGER) -> ?PARSER_NUMBER_TOKEN;
to_parser(?AQL_BOOLEAN) -> ?PARSER_STRING_TOKEN;
to_parser(?AQL_COUNTER_INT) -> ?PARSER_NUMBER_TOKEN;
to_parser(?AQL_VARCHAR) -> ?PARSER_STRING_TOKEN;
to_parser(Invalid) -> throw(lists:concat(["No mapping available for: ", Invalid])).

to_insert_op(?AQL_INTEGER, OpParam) -> crdt:set_integer(OpParam);
to_insert_op(?AQL_BOOLEAN, OpParam) ->
  case OpParam of
    "true" ->
      crdt:enable_flag();
    _Else ->
      crdt:disable_flag()
  end;
to_insert_op(?AQL_COUNTER_INT, OpParam) -> crdt:increment_counter(OpParam);
to_insert_op(?AQL_VARCHAR, OpParam) -> crdt:assign_lww(OpParam);
to_insert_op(Invalid, _OpParam) -> throw(lists:concat(["No mapping available for: ", Invalid])).
