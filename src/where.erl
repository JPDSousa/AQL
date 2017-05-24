
-module(where).

-include("parser.hrl").
-include("aql.hrl").

-export([scan/2]).

scan(TName, Conditions) ->
  scan(TName, Conditions, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

scan(TName, [{?PARSER_ATOM(_ClValue), Arop, {_AQLType, Str}} | T], Acc) ->
	case Arop of
		?PARSER_EQUALITY ->
			NewAcc = lists:flatten(Acc, [element:key(Str, TName)]),
			scan(TName, T, NewAcc);
		_Else ->
			{err, "Not supported yet! :)"}
	end;
scan(_TName, [], Acc) ->
	Acc.
