%% @author Joao
%% @doc @todo Add description to select.


-module(insert).

-define(NO_PK, none).

-include("aql.hrl").
-include("parser.hrl").
-include("types.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/3]).

-export([table/1,
				keys/2,
				values/1]).

exec({Table, Tables}, Props, TxId) ->
	Keys = keys(Props, Table),
	Values = values(Props),
	AnnElement = element:new(Table),
	{ok, Element} = element:put(Keys, Values, AnnElement),
	Element1 = element:build_fks(Element, TxId),
	element:insert(Element1, TxId),
	Pk = element:primary_key(Element1),
	index:put(Pk, TxId),
	% update foreign key references
	%touch_cascade(Element1, Tables, TxId),
	Fks = element:foreign_keys(foreign_keys:from_table(Table), Element1),
	FksKV = read_fks(Fks, Tables, TxId, true),
	lists:foreach(fun ({Fk, Data}) -> touch(Fk, Data, Tables, TxId) end, FksKV).

table({TName, _Keys, _Values}) -> TName.

keys({_TName, Keys, _Values}, Table) ->
	case Keys of
		?PARSER_WILDCARD ->
			column:s_names(Table);
		_Else ->
			Keys
	end.

values({_TName, _Keys, Values}) -> Values.

%% ====================================================================
%% Internal functions
%% ====================================================================

read_fks(Fks, _Tables, TxId, false) ->
	lists:map(fun({_Col, {PTabName, _PTabAttr}, Value} = Fk) ->
		TKey = element:create_key(Value, PTabName),
		{ok, [Data]} = antidote:read_objects(TKey, TxId),
		{Fk, Data}
	end, Fks);
read_fks(Fks, Tables, TxId, true) ->
	lists:map(fun({_Col, {PTabName, _PTabAttr}, Value} = Fk) ->
		TKey = element:create_key(Value, PTabName),
		Table = table:lookup(PTabName, Tables),
		{ok, [Data]} = antidote:read_objects(TKey, TxId),
		case element:is_visible(Data, Table, TxId) of
			false ->
				throw(lists:concat(["Cannot find row ", Value, " in table ", PTabName]));
			_Else ->
				{Fk, Data}
		end
	end, Fks).

touch({_Col, {PTabName, _PTabAttr}, Value}, Data, Tables, TxId) ->
	TKey = element:create_key(Value, PTabName),
	antidote:update_objects(crdt:ipa_update(TKey, ipa:touch()), TxId),
	Table = table:lookup(PTabName, Tables),
	% touch cascade
	touch_cascade(Data, Table, Tables, TxId),
	% touch parents
	Fks = element:foreign_keys(foreign_keys:from_table(Table), Data, PTabName),
	FksKV = read_fks(Fks, Tables, TxId, false),
	lists:foreach(fun ({Fk, Data}) -> touch(Fk, Data, Tables, TxId) end, FksKV).

touch_cascade(Data, Table, Tables, TxId) ->
	TName = table:name(Table),
	Refs = table:dependants(TName, Tables),
	lists:foreach(fun({RefTName, RefCols}) ->
		lists:foreach(fun(?T_FK(FkName, FkType, _TName, CName)) ->
			Value = element:get(CName, types:to_crdt(FkType), Data, Table),
			index:tag(RefTName, FkName, Value, ipa:touch_cascade(), TxId)
	 	end, RefCols)
	end, Refs).
