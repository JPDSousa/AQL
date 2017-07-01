%%%-------------------------------------------------------------------
%% @doc aqlparser public API
%% @end
%%%-------------------------------------------------------------------

-module(aqlparser).

-include("aql.hrl").
-include("parser.hrl").

%% Application callbacks
-export([parse/1, start_shell/0]).

%%====================================================================
%% API
%%====================================================================

-spec parse(input()) -> queryResult() | {err, term()}.
parse({str, Query}) ->
	TokensRes = scanner:string(Query),
	case TokensRes of
		{ok, Tokens, _} ->
			%io:fwrite("~p~n", [Tokens]),
			ParseRes = parser:parse(Tokens),
			case ParseRes of
				{ok, ParseTree} ->
					%io:fwrite("~p~n", [ParseTree]),
					exec(ParseTree, []);
				_Else ->
					ParseRes
			end;
		_Else ->
			TokensRes
	end;
parse({file, Filename}) ->
	{ok, File} = file:read_file(Filename),
	Content = unicode:characters_to_list(File),
	parse({str, Content}).

start_shell() ->
	io:fwrite("Welcome to the AQL Shell.~n"),
	read_and_exec().

read_and_exec() ->
	Line = io:get_line("AQL>"),
	Res = parse({str, Line}),
	io:fwrite(">> ~p~n", [Res]),
	read_and_exec().

%%====================================================================
%% Internal functions
%%====================================================================

exec([Query | Tail], Acc) ->
	Res = exec(Query),
	case Res of
		ok ->
			exec(Tail, Acc);
		Else ->
			exec(Tail, lists:append(Acc, [Else]))
	end;
exec([], Acc) ->
	{ok, Acc}.

exec(?CREATE_CLAUSE(Table)) ->
	eval("Create Table", table:write_table(Table));
exec(?INSERT_CLAUSE(Insert)) ->
	Table = get_table_from_query(Insert),
	eval("Insert", insert:exec(Table, Insert));
exec(?DELETE_CLAUSE(Delete)) ->
	Table = get_table_from_query(Delete),
	eval("Delete", delete:exec(Table, Delete));
exec({?UPDATE_TOKEN, Update}) ->
	Table = get_table_from_query(Update),
	eval("Update", update:exec(Table, Update));
exec({?SELECT_TOKEN, Select}) ->
	Table = get_table_from_query(Select),
	eval("Select", select:exec(Table, Select));
exec(_Invalid) ->
	throw("Invalid query").

eval(Query, Status) ->
	AQuery = list_to_atom(Query),
	case Status of
		ok ->
			io:fwrite("[Ok] ~p~n", [AQuery]),
			Status;
		{ok, Msg} ->
			io:fwrite("[Ok] ~p: ~p~n", [AQuery, Msg]),
			Msg;
		err ->
			io:fwrite("[Err] ~p~n", [AQuery]),
			throw(Query);
		{err, Msg} ->
			io:fwrite("[Err] ~p: ~p~n", [AQuery, Msg]),
			throw(Msg);
		Msg ->
			io:fwrite("[????] ~p: ~p~n", [AQuery, Msg]),
			Msg
	end.

get_table_from_query(Props) ->
	TableName = table:name(Props),
	case table:get_table(TableName) of
		{false, none} ->
			{err, "The table does not exist.~n"};
		Table ->
			Table
	end.
