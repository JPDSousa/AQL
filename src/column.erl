
-module(column).

-include("parser.hrl").
-include("aql.hrl").

-export([name/1,
				constraint/1,
				type/1,
				is_primarykey/1]).

-export([filter_fks/1]).

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

is_primarykey(Col) when ?is_column(Col) ->
	case constraint(Col) of
		?PRIMARY_TOKEN ->
			true;
		_Else ->
			false
	end;
is_primarykey(Col) ->
	is_primarykey(unwrap(Col)).

unwrap(Column) ->
	case Column of
		{?PROP_ATTR, Meta} ->
			Meta;
		Unwrapped ->
			Unwrapped
	end.

%% ====================================================================
%% Column utils functions
%% ====================================================================

filter_fks(Columns) ->
	dict:filter(fun (Col) ->
		case constraint(Col) of
			?FOREIGN_KEY({?PARSER_ATOM(_Table), _Attr}) ->
				true;
			_Else ->
				false
		end
	end, Columns).
