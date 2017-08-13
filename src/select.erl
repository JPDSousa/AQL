%% @author Joao
%% @doc @todo Add description to select.


-module(select).

-include_lib("parser.hrl").
-include_lib("aql.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/3]).

exec({Table, _Tables}, Select, TxId) ->
	TName = table:name(Table),
	Cols = column:s_from_table(Table),
	Projection = proplists:get_value(?PROP_COLUMNS, Select),
	% TODO validate projection fields
	Condition = proplists:get_value(?WHERE_TOKEN, Select),
	Keys = where:scan(TName, Condition, TxId),
	{ok, Results} = antidote:read_objects(Keys, TxId),
	ProjRes = project(Projection, Results, [], Cols),
	ActualRes = apply_offset(ProjRes, Cols, []),
	{ok, ActualRes}.

apply_offset([{{Key, Type}, V} | Values], Cols, Acc) ->
  Col = dict:fetch(Key, Cols),
  Cons = column:constraint(Col),
	case {Type, Cons} of
    {?AQL_COUNTER_INT, {?COMPARATOR_KEY(Comp), ?PARSER_NUMBER(Offset)}} ->
			AQLCounterValue = bcounter:from_bcounter(Comp, V, Offset),
			NewAcc = lists:append(Acc, [{Key, AQLCounterValue}]),
      apply_offset(Values, Cols, NewAcc);
    _Else -> {{Key, Type}, V}
  end;
apply_offset([], _Cols, Acc) -> Acc.


project(Projection, [Result | Results], Acc, Cols) ->
	ProjRes = project_row(Projection, Result, [], Cols),
	project(Projection, Results, Acc ++ ProjRes, Cols);
project(_Projection, [], Acc, _Cols) ->
	Acc.

project_row(?PARSER_WILDCARD, Result, _Acc, _Cols) ->
	Result;
project_row([?PARSER_ATOM(Atom) | Tail], Result, Acc, Cols) ->
	{{Key, _Type}, Value} = get_value(Atom, Result),
	Col = column:s_get(Cols, Key),
	Type = column:type(Col),
	NewResult = proplists:delete(Atom, Result),
	NewAcc = Acc ++ [{{Key, Type}, Value}],
	project_row(Tail, NewResult, NewAcc, Cols);
project_row([], _Result, Acc, _Cols) ->
	Acc.

get_value(Key, [{{Name, _Type}, _Value} = H| T]) ->
	case Key of
		Name ->
			H;
		_Else ->
			get_value(Key, T)
	end;
get_value(_Key, []) ->
	undefined.
