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

-spec parse(input()) -> ok | queryResult().
parse({str, Query}) ->
	TokensRes = scanner:string(Query),
	case TokensRes of
		{ok, Tokens, _} ->
			%io:fwrite("~p~n", [Tokens]),
			ParseRes = parser:parse(Tokens),
			case ParseRes of
				{ok, ParseTree} ->
					%io:fwrite("~p~n", [ParseTree]),
					exec(ParseTree);
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

-spec exec(queries()) -> ok.
exec([Query | Tail]) ->
	exec(Query),
	exec(Tail);
exec([]) ->
	ok;
exec({?CREATE_TOKEN, Table}) ->
	{ok, _CT} = table:write_table(Table);
exec({?INSERT_TOKEN, Insert}) ->
	Table = get_table_from_query(Insert),
	ok = insert:exec(Table, Insert);
exec({?UPDATE_TOKEN, Update}) ->
	Table = get_table_from_query(Update),
	ok = update:exec(Table, Update);
exec({?SELECT_TOKEN, Select}) ->
	Table = get_table_from_query(Select),
	Res = select:exec(Table, Select),
	io:fwrite("~p~n", [Res]).

get_table_from_query(Props) ->
	TableName = table:name(Props),
	case table:get_table(TableName) of
		{false, none} ->
			{err, "The table does not exist.~n"};
		Table ->
			Table
	end.
