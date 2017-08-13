
-module(foreign_keys).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("aql.hrl").
-include("parser.hrl").

-export([from_table/1,
			from_columns/1,
			load_chain/4
			]).

from_column(Column) ->
	Name = column:name(Column),
	Type = column:type(Column),
	Constraint = column:constraint(Column),
	?FOREIGN_KEY({?PARSER_ATOM(TName), ?PARSER_ATOM(Attr)}) = Constraint,
	?T_FK(Name, Type, TName, Attr).

from_table(Table) ->
	from_columns(column:s_from_table(Table)).

from_columns(Columns) ->
	Fks = maps:filter(fun (_CName, Col) ->
		column:is_foreign_key(Col)
	end, Columns),
	FkList = dict:to_list(Fks),
	lists:map(fun ({_Name, Column}) ->
		from_column(Column)
	end, FkList).

load_chain([{CName, TName} | FkChain], Value, Tables, TxId) ->
	Table = table:lookup(TName, Tables),
	Fks = from_table(Table),
	{ok, [Parent]} = antidote:read_objects(element:create_key(Value, TName), TxId),
	Unflat = lists:map(fun ({{_Key, FkType}, {FkTable, FkAttr}}) ->
		FkName = [{FkAttr, FkTable}] ++ [{CName, TName}] ++ FkChain,
		FkValue = element:get(FkAttr, types:to_crdt(FkType), Parent, TName),
		[{FkName, FkType, FkValue} | load_chain(FkName, FkValue, Tables, TxId)]
	end, Fks),
	lists:flatten(Unflat).

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

create_table_aux() ->
  {ok, Tokens, _} = scanner:string("CREATE TABLE tb (id INT PRIMARY KEY, B VARCHAR FOREIGN KEY REFERENCES tb(id), C INT)"),
	{ok, [{?CREATE_TOKEN, Table}]} = parser:parse(Tokens),
  Table.

from_table_test() ->
	Table = create_table_aux(),
	Expected = [{{'B', ?AQL_VARCHAR}, {tb, id}}],
	?assertEqual(Expected, from_table(Table)).

from_columns_test() ->
	Table = create_table_aux(),
	Cols = column:s_from_table(Table),
	Expected = [{{'B', ?AQL_VARCHAR}, {tb, id}}],
	?assertEqual(Expected, from_columns(Cols)).

parents_test() ->
  FKs = [
		{{fka, crdt}, {tableA, id}},
		{{fkb, crdt}, {tableB, id}}
  ],
  Element = [
		{{a, crdt}, valueA},
		{{b, crdt}, valueB},
		{{fka, crdt}, valueFKa},
		{{fkb, crdt}, valueFKb}
	],
	Expected = [
		element:create_key(valueFKa, tableA),
		element:create_key(valueFKb, tableB)
	],
	?assertEqual(Expected, parents(Element, FKs)).

-endif.
