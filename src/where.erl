
-module(where).

-export([scan/3]).

scan(TableName, Keys, Conditions) ->
  scan(TableName, Keys, Conditions, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

scan(TName, Cls, [{{atom_value, _ClName}, Arop, {_AQLType, Str}} | T], Acc) ->
	case Arop of
		{assignment, "="} ->
			NewAcc = lists:append(Acc, element:new(Str, TName)),
			scan(TName, Cls, T, NewAcc);
		_Else ->
			{err, "Not supported yet! :)"}
	end;
scan(_TName, _Cls, [], Acc) ->
	{ok, Acc}.
