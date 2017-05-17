
-module(where).

-export([scan/2]).

scan(TableName, Condition) ->
  scan(TableName, Condition, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

scan(TableName, [{{atom_value, _Left}, Arop, {_AQLType, Str}} | Tail], Acc) ->
	case Arop of
		{assignment, "="} ->
			NewAcc = lists:append(Acc, element:new(Str, TableName)),
			scan(TableName, Tail, NewAcc);
		_Else ->
			{err, "Not supported yet! :)"}
	end;
scan(_TName, [], Acc) ->
	{ok, Acc}.
