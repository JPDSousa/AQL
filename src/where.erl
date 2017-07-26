
-module(where).

-include("parser.hrl").
-include("aql.hrl").

-export([scan/3]).

scan(TName, undefined, TxId) ->
  %TODO scan all
  index:keys(TName, TxId);
scan(TName, Conditions, _TxId) ->
  evaluate(TName, Conditions, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

evaluate(TName, [{?PARSER_ATOM(_ClValue), Arop, {_AQLType, Str}} | T], Acc) ->
	case Arop of
		?PARSER_EQUALITY ->
			NewAcc = lists:flatten(Acc, [element:create_key(Str, TName)]),
			evaluate(TName, T, NewAcc);
		_Else ->
			throw("Not supported yet! :)")
	end;
evaluate(_TName, [], Acc) ->
	Acc.
