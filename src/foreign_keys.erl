
-module(foreign_keys).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("aql.hrl").
-include("parser.hrl").

-export([parents/2,
			from_table/1,
			from_columns/1,
			load_chain/4
			]).

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

parents(Element, FKs) ->
	lists:map(fun ({K, {Table, _Col}}) ->
		Value = proplists:get_value(K, Element),
		element:create_key(Value, Table)
	end, FKs).

from_table(Table) ->
	from_columns(column:s_from_table(Table)).

from_columns(Columns) ->
	F = dict:filter(fun (_ColName, Col) ->
		case column:constraint(Col) of
			?FOREIGN_KEY({?PARSER_ATOM(_Table), _Attr}) ->
				true;
			_Else ->
				false
		end
	end, Columns),
	List = dict:to_list(F),
	lists:map(fun ({Name, Column}) ->
		Type = column:type(Column),
		?FOREIGN_KEY({?PARSER_ATOM(TName), ?PARSER_ATOM(Attr)}) = column:constraint(Column),
		{{Name, Type}, {TName, Attr}}
	end, List).


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
