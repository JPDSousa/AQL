%% @author Joao
%% @doc @todo Add description to tables.

-module(table).

-include("parser.hrl").
-include("aql.hrl").
-include("types.hrl").

-define(TABLE_META, ?BOUND_OBJECT('#tables', antidote_crdt_gmap, ?METADATA_BUCKET)).
-define(CRDT_TYPE, antidote_crdt_lwwreg).

-export([exec/2]).

-export([read_tables/1,
				write_table/2,
				lookup/2, lookup/3,
				dependents/2]).

-export([name/1,
				policy/1,
				columns/1,
				shadow_columns/1]).

exec(Table, TxId) ->
	write_table(Table, TxId).

%% ====================================================================
%% Read/Write functions
%% ====================================================================

read_tables(TxId) ->
	{ok, [Tables]} = antidote:read_objects(?TABLE_META, TxId),
	Tables.

write_table(RawTable, TxId) when ?is_table(Table) ->
	Tables = read_tables(TxId),
	Table = prepare_table(RawTable),
	TableUpdate = create_table_update(Table),
	antidote:update_objects(TableUpdate, TxId).

prepare_table(Table) ->
	Table1 = prepare_cols(Table),
	prepare_foreign_keys(Table1, Tables).

prepare_cols(Table) ->
	RawCols = columns(Table),
	Builder = lists:foldl(fun columns_builder:put_raw/2, columns_builder:new(), RawCols),
	set_columns(columns_builder:build(Builder), Table).

prepare_foreign_keys(Table, Tables) ->
	FKs = foreign_keys:from_table(Table),
	ShadowCols = lists:map(fun ({_K, {TName, Attr}}) ->
		Err1 = ["Table ", TName, " in foreign key reference does not exist."],
		Err2 = ["Column ", Attr, " does not exist in table ", TName],
		T = lookup(TName, Tables, lists:concat(Err1)),
		Col = column:s_get(T, Attr, lists:concat(Err2)),
		% TODO fetch foreign keys and create shadow cols from that
		% TODO fetch shadow columns and create shadow cols from that
		case column:is_primarykey(Col) of
			false ->
				throw("Foreign keys can only reference unique columns");
			_Else ->
				ok
		end
	end, FKs),
	Table.


create_table_update(Name, Table) ->
	Op = crdt:assign_lww(Table),
	crdt:single_map_update(?TABLE_META, Name, ?CRDT_TYPE, Op).

lookup(Name, Tables, ErrMsg) ->
	NameAtom = utils:to_atom(Name),
	Res = proplists:get_value({NameAtom, ?CRDT_TYPE}, Tables),
	case Res of
		undefined ->
			throw(ErrMsg);
		_Else ->
			Res
	end.

lookup(Name, Tables) when is_list(Tables) ->
	ErrMsg = lists:concat(["No such table: ", Name]),
	lookup(Name, Tables, ErrMsg);
lookup(Name, TxId) ->
	Tables = read_tables(TxId),
	lookup(Name, Tables).

% dependents(TName, Tables) when is_list(Tables) ->
% 	{Table, NewTables} = utils:seek_and_destroy(TName, Tables),
% 	dependents(TName, NewTables, Table, []).
% dependents(TName, TxId) ->
% 	dependents(TName, read_tables(TxId)).
%
% dependents(TName, Tables, Current, FlatTree) ->
%

%% ====================================================================
%% Table Props functions
%% ====================================================================

name(?T_TABLE(Name, _Policy, _Cols, _SCols)) -> Name.

policy(?T_TABLE(_Name, Policy, _Cols, _SCols)) -> Policy.

set_policy(Policy, ?T_TABLE(Name, _Policy, Cols, SCols)) ->
	?T_TABLE(Name, Policy, Cols, SCols).

columns(?T_TABLE(_Name, _Policy, Cols, _SCols)) -> Cols.

set_columns(Cols, ?T_TABLE(Name, Policy, _Cols, SCols)) ->
	?T_TABLE(Name, Policy, Cols, SCols).

shadow_columns(?T_TABLE(_Name, _Policy, _Cols, SCols)) -> SCols.

set_shadow_columns(SCols, ?T_TABLE(Name, Policy, Cols, _SCols)) ->
	?T_TABLE(Name, Policy, Cols, SCols).

%% ====================================================================
%% Internal functions
%% ====================================================================
