
-module(query_utils).

-export([search_clause/2]).

search_clause(Name, [{ClauseName, Clause} | Tail]) ->
  case ClauseName of
    Name ->
      Clause;
    _Else ->
      search_clause(Name, Tail)
    end;
search_clause(Name, []) ->
  {err, lists:concat(["Could not resolve ", Name, " clause"])}.
