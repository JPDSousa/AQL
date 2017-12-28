
-module(foreign_keys).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("parser.hrl").
-include_lib("aql.hrl").
-include_lib("types.hrl").

-export([from_table/1,
			from_columns/1,
			to_cname/1]).

from_table(Table) ->
	from_columns(table:columns(Table)).

from_column(Column) ->
	Name = column:name(Column),
	Type = column:type(Column),
	Constraint = column:constraint(Column),
	?FOREIGN_KEY({TName, Attr}) = Constraint,
	?T_FK(Name, Type, TName, Attr).

from_columns(Columns) ->
	Fks = maps:filter(fun (_CName, Col) ->
		column:is_foreign_key(Col)
	end, Columns),
	FkList = maps:to_list(Fks),
	lists:map(fun ({_Name, Column}) ->
		from_column(Column)
	end, FkList).

to_cname([{_TName, CName}]) -> CName;
to_cname(ShCName) -> ShCName.

%load_chain([{CName, TName} | FkChain], Value, Tables, TxId) ->
%	Table = table:lookup(TName, Tables),
%	Fks = from_table(Table),
%	{ok, [Parent]} = antidote:read_objects(element:create_key(Value, TName), TxId),
%	Unflat = lists:map(fun ({{_Key, FkType}, {FkTable, FkAttr}}) ->
%		FkName = [{FkAttr, FkTable}] ++ [{CName, TName}] ++ FkChain,
%		FkValue = element:get(FkAttr, types:to_crdt(FkType), Parent, TName),
%		[{FkName, FkType, FkValue} | load_chain(FkName, FkValue, Tables, TxId)]
%	end, Fks),
%	lists:flatten(Unflat).

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

-endif.
