
-module(column).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("parser.hrl").
-include("aql.hrl").
-include("types.hrl").

-export([name/1,
				constraint/1,
				type/1,
				is_primary_key/1,
				is_default/1,
				is_foreign_key/1]).

-export([unwrap_name/1,
					unwrap_type/1,
					unwrap_constraint/1]).

-export([s_primary_key/1,
				s_filter_defaults/1,
				s_get/2, s_get/3,
				s_names/1]).

%% ====================================================================
%% Column props functions
%% ====================================================================

name(?T_COL(Name, _, _)) -> Name.

constraint(?T_COL(_, _, Constraint)) -> Constraint.

type(?T_COL(_, Type, _)) -> Type.

is_primary_key(?T_COL(_, _, ?PRIMARY_TOKEN)) -> true;
is_primary_key(_) -> false.

is_default(?T_COL(_, _, ?DEFAULT_KEY(_V))) -> true;
is_default(_) -> false.

is_foreign_key(?T_COL(_, _, ?FOREIGN_KEY(_V))) -> true;
is_foreign_key(_) -> false.

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

s_primary_key(Table) when ?is_table(Table) ->
	Columns = table:columns(Table),
	s_primary_key(Columns);
s_primary_key(Columns) ->
	PkNames = maps:get(?C_PK, Columns),
	lists:map(fun(PkName) ->
		maps:get(PkName, Columns)
	end, PkNames).

s_filter_defaults(Table) when ?is_table(Table) ->
	s_filter_defaults(table:columns(Table));
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
	maps:get(?C_NAMES, Cols).

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

pk() -> ?PRIMARY_TOKEN.
def() -> ?DEFAULT_KEY("Test").
fk() -> ?FOREIGN_KEY({"TName", "TCol"}).
check() -> ?CHECK_KEY({"<", 3}).
no() -> ?NO_CONSTRAINT.

name_test() ->
	Expected = test,
	?assertEqual(Expected, name(?T_COL(Expected, a, a))).

constraint_test() ->
	Expected = check(),
	?assertEqual(Expected, constraint(?T_COL(a, a, Expected))).

type_test() ->
	Expected = test,
	?assertEqual(Expected, type(?T_COL(a, Expected, a))).

is_primary_key_test() ->
	?assertEqual(true, is_primary_key(?T_COL(a, a, pk()))),
	?assertEqual(false, is_primary_key(?T_COL(a, a, def()))),
	?assertEqual(false, is_primary_key(?T_COL(a, a, fk()))),
	?assertEqual(false, is_primary_key(?T_COL(a, a, check()))),
	?assertEqual(false, is_primary_key(?T_COL(a, a, no()))).

is_default_test() ->
	?assertEqual(false, is_default(?T_COL(a, a, pk()))),
	?assertEqual(true, is_default(?T_COL(a, a, def()))),
	?assertEqual(false, is_default(?T_COL(a, a, fk()))),
	?assertEqual(false, is_default(?T_COL(a, a, check()))),
	?assertEqual(false, is_default(?T_COL(a, a, no()))).

is_foreign_key_test() ->
	?assertEqual(false, is_foreign_key(?T_COL(a, a, pk()))),
	?assertEqual(false, is_foreign_key(?T_COL(a, a, def()))),
	?assertEqual(true, is_foreign_key(?T_COL(a, a, fk()))),
	?assertEqual(false, is_foreign_key(?T_COL(a, a, check()))),
	?assertEqual(false, is_foreign_key(?T_COL(a, a, no()))).

-endif.