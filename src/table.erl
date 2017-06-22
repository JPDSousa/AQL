%% @author Joao
%% @doc @todo Add description to tables.

-module(table).

-define(BOUND_OBJECT, {'#tables1', antidote_crdt_gmap, aql_metadata}).
-define(CRDT_TYPE, antidote_crdt_lwwreg).

-include("parser.hrl").
-include("aql.hrl").

-export([read_tables/0,
				write_table/1,
				get_table/1, get_table/2]).

-export([get_column/2, get_columns/1,
				primary_key/1,
				name/1]).

%% ====================================================================
%% Read/Write functions
%% ====================================================================

read_tables() ->
	{ok, [TablesMeta], _CommitTime} = antidote:read_objects(?BOUND_OBJECT),
	UnwrappedTables = lists:map(fun unwrap_pair/1, TablesMeta),
	{ok, UnwrappedTables}.

unwrap_pair(Table) ->
	case Table of
		{_column, Value} ->
			Value;
		_Else ->
			Table
	end.

write_table(Table) when ?is_table(Table) ->
	Name = name(Table),
	TableUpdate = create_table_update(Name, Table),
	antidote:update_objects(TableUpdate).

create_table_update(Name, Table) when ?is_tname(Name) and ?is_table(Table) ->
	Op = {assign, Table},
	crdt:single_map_update(?BOUND_OBJECT, Name, ?CRDT_TYPE, Op).

get_table(Name) when ?is_tname(Name) ->
	{ok, Tables} = read_tables(),
	get_table(Tables, Name).

get_table([Table | Tail], Name) when ?is_table(Table) and ?is_tname(Name) ->
	TName = name(Table),
	case TName of
		Name ->
			Table;
		_Else ->
			get_table(Tail, Name)
	end;
get_table([], _) ->
	{false, none}.

%% ====================================================================
%% Table Props functions
%% ====================================================================

name(Table) when ?is_table(Table) ->
	TableName = query_utils:search_clause(?PROP_TABLE_NAME, Table),
  case TableName of
    {err, ErrMsg} ->
      {err, ErrMsg};
    _Else ->
			?PARSER_ATOM(Name) = TableName,
			Name
  end.

get_column(Columns, ColumnName) when ?is_cname(ColumnName) ->
	Res = maps:get(ColumnName, Columns),
	case Res of
		{badkey, _ColumnName} ->
			{err, lists:concat(["Collumn ", ColumnName, " does not exist"])};
		_Else ->
			{ok, Res}
	end;
get_column(Table, Column) when ?is_table(Table) and ?is_cname(Column) ->
	Columns = get_columns(Table),
	get_column(Columns, Column).

get_columns(Table) when ?is_table(Table)->
	CList = query_utils:search_clause(?PROP_COLUMNS, Table),
	case CList of
		{err, _ErrMsg} ->
			CList;
		_Else ->
			list_to_dict(CList, fun column:name/1)
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
