
-module(column).

-include("parser.hrl").
-include("aql.hrl").

-export([name/1,
				constraint/1,
				type/1]).

name([?PROP_ATTR_NAME(?PARSER_ATOM(Name)) | _]) ->
  Name;
name([_|Tail]) ->
  name(Tail);
name([]) ->
  {err, "Could not resolve column name"}.

constraint([?PROP_ATTR_CONSTRAINT(Constraint) | _]) ->
  Constraint;
constraint([_|Tail]) ->
  constraint(Tail);
constraint([]) ->
  {err, "Could not resolve column constraint"}.

type([{?ATTR_TYPE_TOKEN, AqlType} | _]) ->
  AqlType;
type([_|Tail]) ->
  type(Tail);
type([]) ->
  {err, "Could not resolve column type"}.
