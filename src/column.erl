
-module(column).

-include("parser.hrl").
-include("aql.hrl").
-include("types.hrl").

-export([name/1,
				constraint/1,
				type/1,
				is_primarykey/1,
				is_default/1,
				is_foreign_key/1]).

-export([unwrap_name/1,
					unwrap_type/1,
					unwrap_constraint/1]).

-export([s_from_table/1,
				s_primary_key/1,
				s_filter_defaults/1,
				s_get/2, s_get/3,
				s_names/1]).

%% ====================================================================
%% Column props functions
%% ====================================================================

name(?T_COL(Name, _, _)) -> Name.

constraint(?T_COL(_, _, Constraint)) -> Constraint.

type(?T_COL(_, Type, _)) -> Type.

is_primarykey(?T_COL(_, _, ?PRIMARY_TOKEN)) -> true;
is_primarykey(_) -> false;

is_default(?T_COL(_, _, {?DEFAULT_TOKEN, _V})) -> true;
is_default(_) -> false;

is_foreign_key(?T_COL(_, _, ?FOREIGN_KEY(_V))) -> true;
is_foreign_key(_) -> true;

unwrap_name(?PARSER_ATOM(Name)) -> Name.

unwrap_type(?ATTR_KEY(Type)) -> Type.

unwrap_constraint(?FOREIGN_KEY({?PARSER_ATOM(Table), ?PARSER_ATOM(Attr)})) ->
	?FOREIGN_KEY({Table, Attr});
unwrap_constraint(?DEFAULT_KEY({?PARSER_TYPE(_Type, Value)})) ->
	?DEFAULT_KEY({Value});
unwrap_constraint(Constraint) -> Constraint
%% ====================================================================
%% Columns Utilities
%% ====================================================================

s_from_table(Table) ->
	table:columns(Table).

s_primary_key(Table) when ?is_table(Table) ->
	Columns = table:columns(Table),
	s_primary_key(Columns);
s_primary_key(Columns) ->
	PkNames = maps:get(?C_PK, Columns),
	lists:map(fun(PkName) ->
		maps:get(PkName, Columns)
	end, PkNames).

s_filter_defaults(Columns) when is_list(Columns) ->
	lists:filter(fun is_default/1, Columns);
s_filter_defaults(Columns) ->
	maps:filter(fun (_K, V) -> is_default(V) end, Columns).

s_get(Table, Column) when ?is_table(Table) ->
	Columns = table:columns(Table),
	s_get(Columns, Column);
s_get(Columns, ColumnName) ->
	ErrMsg = lists:concat(["Collumn ", ColumnName, " does not exist"]),
	s_get(Columns, ColumnName, ErrMsg).

s_get(Table, CName, ErrMsg) when ?is_table(Table) ->
	Columns = table:columns(Table),
	s_get(Columns, CName, ErrMsg);
s_get(Cols, CName, ErrMsg) ->
	Res = maps:find(CName, Cols),
	case Res of
		{ok, Column} ->
			Column;
		_Else ->
			throw(ErrMsg)
	end.

s_names(Table) when ?is_table(Table) ->
	s_names(table:columns(Table));
s_names(Cols) when is_map(Cols) ->
	maps:get(?NAMES, Cols).
