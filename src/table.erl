%% @author Joao
%% @doc @todo Add description to tables.

-module(table).

-define(BOUND_OBJECT, {'#tables1', antidote_crdt_gmap, aql_metadata}).
-define(CRDT_TYPE, antidote_crdt_lwwreg).

-include("parser.hrl").
-include("aql.hrl").

-export([exec/2]).

-export([read_tables/1,
				write_table/2,
				lookup/2]).

-export([get_column/2, get_columns/1, get_col_names/1,
				primary_key/1,
				name/1]).

%% ====================================================================
%% Read/Write functions
%% ====================================================================

exec(Table, TxId) ->
	write_table(Table, TxId).

read_tables(TxId) ->
	{ok, [Tables]} = antidote:read_objects(?BOUND_OBJECT, TxId),
	Tables.

write_table(Table, TxId) when ?is_table(Table) ->
	check_foreign_keys(Table, TxId),
	Name = name(Table),
	TableUpdate = create_table_update(Name, Table),
	antidote:update_objects(TableUpdate, TxId).

check_foreign_keys(Table, TxId) ->
	Tables = read_tables(TxId),
	FKs = foreign_keys:from_table(Table),
	lists:foreach(fun ({_K, {TName, Attr}}) ->
		Err1 = ["Table ", TName, " in foreign key reference does not exist."],
		T = lookup(TName, Tables, lists:concat(Err1)),
		Err2 = ["Column ", Attr, " does not exist in table ", TName],
		Col = get_column(T, Attr, lists:concat(Err2)),
		case column:is_primarykey(Col) of
			false ->
				throw("Foreign keys can only reference unique columns");
			_Else ->
				ok
		end
	end, FKs).


create_table_update(Name, Table) when ?is_tname(Name) and ?is_table(Table) ->
	Op = crdt:assign_lww(Table),
	crdt:single_map_update(?BOUND_OBJECT, Name, ?CRDT_TYPE, Op).

lookup(Name, Tables, ErrMsg) when ?is_tname(Name) and is_list(Tables) ->
	Res = proplists:get_value({Name, ?CRDT_TYPE}, Tables),
	case Res of
		undefined ->
			throw(ErrMsg);
		_Else ->
			Res
	end.

lookup(Name, TxId) when ?is_tname(Name) ->
	Tables = read_tables(TxId),
	lookup(Name, Tables, "No such table").

%% ====================================================================
%% Table Props functions
%% ====================================================================

name(Table) when ?is_table(Table) ->
	TableName = proplists:get_value(?PROP_TABLE_NAME, Table),
  case TableName of
    {err, ErrMsg} ->
      {err, ErrMsg};
    _Else ->
			?PARSER_ATOM(Name) = TableName,
			Name
  end.

get_column(Table, Column) when ?is_table(Table) and ?is_cname(Column) ->
	Columns = get_columns(Table),
	get_column(Columns, Column);
get_column(Columns, ColumnName) when ?is_cname(ColumnName) ->
	ErrMsg = lists:concat(["Collumn ", ColumnName, " does not exist"]),
	get_column(Columns, ColumnName, ErrMsg).

get_column(Table, CName, ErrMsg) when ?is_table(Table) and ?is_cname(CName) ->
	Columns = get_columns(Table),
	get_column(Columns, CName, ErrMsg);
get_column(Cols, CName, ErrMsg) when ?is_cname(CName) ->
	Res = dict:find(CName, Cols),
	case Res of
		{ok, Column} ->
			Column;
		_Else ->
			throw(ErrMsg)
	end.


get_columns(Table) when ?is_table(Table)->
	CList = proplists:get_value(?PROP_COLUMNS, Table),
	case CList of
		{err, _ErrMsg} ->
			CList;
		_Else ->
			list_to_dict(CList, fun column:name/1)
	end.

get_col_names(Table) when ?is_table(Table) ->
	CList = proplists:get_value(?PROP_COLUMNS, Table),
	case CList of
		{err, _ErrMsg} ->
			CList;
		_Else ->
			lists:map(fun column:name/1, CList)
	end.

list_to_dict(List, KeyMapper) ->
	list_to_dict(List, KeyMapper, dict:new()).

list_to_dict([Value | T], KeyMapper, Acc) ->
	Key = KeyMapper(Value),
	NewMap = dict:store(Key, Value, Acc),
	list_to_dict(T, KeyMapper, NewMap);
list_to_dict([], _KeyMapper, Acc) ->
	Acc.

primary_key([{?PROP_ATTR, ColumnData} | Tail]) when ?is_column(ColumnData) ->
	Constraint = column:constraint(ColumnData),
	case Constraint of
		?PRIMARY_TOKEN ->
			ColumnData;
		_Else ->
			primary_key(Tail)
	end;
primary_key([]) ->
	{err, "Could not find primary key"};
primary_key(Table) when ?is_table(Table) ->
	Columns = get_columns(Table),
	case Columns of
		{err, _ErrMsg} ->
			Columns;
		_Else ->
			primary_key(Columns)
	end;
primary_key(Columns) ->
	ColList = dict:to_list(Columns),
	Values = lists:map(fun ({_Key, Value}) -> Value end, ColList),
	primary_key(Values).

%% ====================================================================
%% Internal functions
%% ====================================================================
