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
				dependants/2,
				prepare_table/2]).

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

write_table(RawTable, TxId) ->
	Tables = read_tables(TxId),
	Table = prepare_table(RawTable, Tables),
	TableUpdate = create_table_update(Table),
	antidote:update_objects(TableUpdate, TxId).

prepare_table(Table, Tables) ->
	Table1 = prepare_cols(Table),
	prepare_foreign_keys(Table1, Tables).

prepare_cols(Table) ->
	RawCols = columns(Table),
	Builder = lists:foldl(fun columns_builder:put_raw/2, columns_builder:new(), RawCols),
	set_columns(columns_builder:build(Builder), Table).

prepare_foreign_keys(Table, Tables) ->
	TName = table:name(Table),
	FKs = foreign_keys:from_table(Table),
	ShadowCols = lists:map(fun (?T_FK(FkName, FkType, T1TName, T1CName)) ->
		ShFk = ?T_FK([{TName, FkName}], FkType, T1TName, T1CName),
		Err1 = ["Table ", T1TName, " in foreign key reference does not exist."],
		Err2 = ["Column ", T1CName, " does not exist in table ", T1TName],
		TargetTable = lookup(T1TName, Tables, lists:concat(Err1)),
		TargetCol = column:s_get(TargetTable, T1CName, lists:concat(Err2)),
		case column:is_primary_key(TargetCol) of
			false -> throw("Foreign keys can only reference unique columns");
			_Else ->
				ParentFks = lists:map(fun(?T_FK(TFkName, TFKType, TFKTName, TFKTColName)) ->
					TFKName1 = lists:append([{TName, FkName}], TFkName),
					?T_FK(TFKName1, TFKType, TFKTName, TFKTColName)
				end, shadow_columns(TargetTable)),
				lists:append([ShFk], ParentFks)
		end
	end, FKs),
	set_shadow_columns(lists:flatten(ShadowCols), Table).

create_table_update(Table) ->
	Name = name(Table),
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

dependants(TName, Tables) ->
	dependants(TName, Tables, []).

dependants(TName, [{TName, _Table} | Tables], Acc) ->
	dependants(TName, Tables, Acc);
dependants(TName, [{{T1TName, _}, Table} | Tables], Acc) ->
	Fks = shadow_columns(Table),
	Refs = references(TName, Fks, []),
	case Refs of
		[] ->
			dependants(TName, Tables, Acc);
		_Else ->
			dependants(TName, Tables, lists:append(Acc, [{T1TName, Refs}]))
	end;
dependants(_TName, [], Acc) -> Acc.

references(TName, [?T_FK(_, _, TName, _) = Fk | Fks], Acc) ->
	references(TName, Fks, lists:append(Acc, [Fk]));
references(TName, [_ | Fks], Acc) ->	references(TName, Fks, Acc);
references(_TName, [], Acc) -> Acc.

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
