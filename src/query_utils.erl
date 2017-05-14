
-module(query_utils).

-export([search_clause/2, table_name/1]).

search_clause(Name, [{ClauseName, Clause} | Tail]) ->
  case ClauseName of
    Name ->
      {ok, Clause};
    _Else ->
      search_clause(Name, Tail)
    end;
search_clause(Name, []) ->
  {err, io:format("Could not resolve ~p clause", Name)}.

table_name(Props) ->
  TableName = search_clause(table, Props),
  case TableName of
    {ok, {atom_value, Name}} ->
      {ok, Name};
    _Else ->
      TableName
  end.
