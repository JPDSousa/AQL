
-module(column).

-include("parser.hrl").
-include("aql.hrl").

-export([name/1,
				constraint/1,
				type/1,
				is_primarykey/1,
				is_default/1,
				is_foreign_key/1]).

-export([s_primary_key/1,
				s_from_table/1,
				s_filter_defaults/1,
				s_get/2, s_get/3,
				s_names/1]).

%% ====================================================================
%% Column props functions
%% ====================================================================

name([?PROP_ATTR_NAME(?PARSER_ATOM(Name)) | _]) ->
  Name;
name([_|Tail]) ->
  name(Tail);
name([]) ->
  {err, "Could not resolve column name"};
name(Column) ->
	name(unwrap(Column)).

constraint([?PROP_ATTR_CONSTRAINT(Constraint) | _]) ->
  Constraint;
constraint([_|Tail]) ->
  constraint(Tail);
constraint([]) ->
  {err, "Could not resolve column constraint"};
constraint(Column) ->
	constraint(unwrap(Column)).

type([{?ATTR_TYPE_TOKEN, AqlType} | _]) ->
  AqlType;
type([_|Tail]) ->
  type(Tail);
type([]) ->
  {err, "Could not resolve column type"};
type(Column) ->
	type(unwrap(Column)).

is_primarykey(Col) ->
	case constraint(unwrap(Col)) of
		?PRIMARY_TOKEN -> true;
		_Else -> false
	end.

is_default(Col) ->
	case constraint(unwrap(Col)) of
	 	{?DEFAULT_TOKEN, _Value} -> true;
  	_Else -> false
	end.

is_foreign_key(Col) ->
	case constraint(unwrap(Col)) of
		?FOREIGN_KEY(_Value) -> true;
		_Else -> false
	end.

unwrap(Column) ->
	case Column of
		{?PROP_ATTR, Meta} ->
			Meta;
		Unwrapped ->
			Unwrapped
	end.

%% ====================================================================
%% Columns Utilities
%% ====================================================================

s_from_table(Table) when ?is_table(Table)->
	CList = proplists:get_value(?PROP_COLUMNS, Table),
	case CList of
		undefined ->
			throw("Cannot extract columns");
		_Else ->
			utils:list_to_dict(CList, fun column:name/1)
	end.

s_primary_key(Columns) when is_list(Columns) ->
	Pks = lists:filter(fun is_primarykey/1, Columns),
	case Pks of
		[Pk] -> Pk;
		[] -> throw("Could not find primary key");
		_Else -> throw("Multiple primary keys")
	end;
s_primary_key(Table) when ?is_table(Table) ->
	Columns = s_from_table(Table),
	s_primary_key(Columns);
s_primary_key(Columns) ->
	Pks = lists:filter(fun is_primarykey/1, Columns),
	Size = dict:size(Pks),
	case Size of
		1 -> [{_K, _V}] = dict:to_list(Pks);
		0 -> throw("Could not find primery key");
		_Else -> throw("Multiple primary keys")
	end.

s_filter_defaults(Columns) when is_list(Columns) ->
	lists:filter(fun is_default/1, Columns);
s_filter_defaults(Columns) ->
	dict:filter(fun (_K, V) -> is_default(V) end, Columns).

s_get(Table, Column) when is_list(Table) ->
	Columns = s_from_table(Table),
	s_get(Columns, Column);
s_get(Columns, ColumnName) ->
	ErrMsg = lists:concat(["Collumn ", ColumnName, " does not exist"]),
	s_get(Columns, ColumnName, ErrMsg).

s_get(Table, CName, ErrMsg) when is_list(Table) ->
	Columns = s_from_table(Table),
	s_get(Columns, CName, ErrMsg);
s_get(Cols, CName, ErrMsg) ->
	Res = dict:find(CName, Cols),
	case Res of
		{ok, Column} ->
			Column;
		_Else ->
			throw(ErrMsg)
	end.

s_names(Table) when is_list(Table) ->
	CList = proplists:get_value(?PROP_COLUMNS, Table),
	case CList of
		undefined ->
			throw("Cannot extract columns");
		_Else ->
			lists:map(fun column:name/1, CList)
	end.
