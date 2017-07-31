%% @author Joao
%% @doc @todo Add description to tables.

-module(table).

-include("parser.hrl").
-include("aql.hrl").

-define(BOUND_OBJECT, {'#tables', antidote_crdt_gmap, ?METADATA_BUCKET}).
-define(CRDT_TYPE, antidote_crdt_lwwreg).

-export([exec/2]).

-export([read_tables/1,
				write_table/2,
				lookup/2, lookup/3]).

-export([name/1]).

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
		Err2 = ["Column ", Attr, " does not exist in table ", TName],
		T = lookup(TName, Tables, lists:concat(Err1)),
		Col = column:s_get(T, Attr, lists:concat(Err2)),
		case column:is_primarykey(Col) of
			false ->
				throw("Foreign keys can only reference unique columns");
			_Else ->
				ok
		end
	end, FKs).


create_table_update(Name, Table) ->
	Op = crdt:assign_lww(Table),
	crdt:single_map_update(?BOUND_OBJECT, Name, ?CRDT_TYPE, Op).

lookup(Name, Tables, ErrMsg) ->
	Res = proplists:get_value({Name, ?CRDT_TYPE}, Tables),
	case Res of
		undefined ->
			throw(ErrMsg);
		_Else ->
			Res
	end.

loopup(Name, Tables) when is_list(Tales) ->
	ErrMsg = lists:concat(["No such table: ", Name]),
	loopup(Name, Tables, ErrMsg);
lookup(Name, TxId) when ?is_tname(Name) ->
	Tables = read_tables(TxId),
	lookup(Name, Tables).

%% ====================================================================
%% Table Props functions
%% ====================================================================

name(Table) ->
	TableName = proplists:get_value(?PROP_TABLE_NAME, Table),
  case TableName of
    {err, _ErrMsg} ->
      TableName;
    _Else ->
			?PARSER_ATOM(Name) = TableName,
			Name
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================
