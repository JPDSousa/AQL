
-module(delete).

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/3, table/1, where/1]).

-include("parser.hrl").
-include("aql.hrl").
-include("types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

exec({Table, Tables}, Props, TxId) ->
	TName = table(Props),
	Condition = where(Props),
	Keys = where:scan(TName, Condition, TxId),
	lists:foreach(fun (Key) ->
		antidote:update_objects(crdt:ipa_update(Key, ipa:delete()), TxId),
		delete_cascade(Key, TName, Tables, TxId)
	end, Keys).

table({TName, _Where}) -> TName.

where({_TName, Where}) -> Where.

%% ====================================================================
%% Internal functions
%% ====================================================================

delete_cascade(Key, TName, Tables, TxId) ->
	{ok, [Data]} = antidote:read_objects(Key, TxId),
	DepTables = table:dependents(TName, Tables),
	Ops = lists:map(fun({DepTName, DepCols}) ->
		lists:map(fun({[{_TName, CName} | Chain]} = DepCol) ->
			Value = element:get_by_name(CName, Data),
			index:tag(DepTName, DepCol, Value, ipa:delete_cascade())
		end, DepCols)
	end, DepTables),
	case Ops of
		[] -> ok;
		_Else ->
			antidote:update_objects(lists:flatten(Ops), TxId)
	end.
