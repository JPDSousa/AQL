
-module(where).

-include("parser.hrl").
-include("aql.hrl").

-export([scan/3]).

scan(TableName, Keys, Conditions) ->
  scan(TableName, Keys, Conditions, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

scan(Table, Cls, [{?PARSER_ATOM(_ClValue), Arop, {_AQLType, Str}} | T], Acc) when ?is_table(Table)->
	case Arop of
		?PARSER_EQUALITY ->
      TName = table:name(Table),
			NewAcc = lists:flatten(Acc, [element:new(Str, Table)]),
			scan(TName, Cls, T, NewAcc);
		_Else ->
			{err, "Not supported yet! :)"}
	end;
scan(_TName, _Cls, [], Acc) ->
	{ok, Acc}.
