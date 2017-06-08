%%%-------------------------------------------------------------------
%% @doc aqlparser public API
%% @end
%%%-------------------------------------------------------------------

-module(aqlparser).

-include("aql.hrl").
-include("parser.hrl").

%% Application callbacks
-export([parse/1]).

%%====================================================================
%% API
%%====================================================================

-spec parse(input()) -> ok | queryResult().
parse({str, Query}) ->
	{ok, Tokens, _} = scanner:string(Query),
	%io:fwrite("~p~n", [Tokens]),
	{ok, ParseTree} = parser:parse(Tokens),
	%io:fwrite("~p~n", [ParseTree]),
	exec(ParseTree);
parse({file, Filename}) ->
	{ok, File} = file:read_file(Filename),
	Content = unicode:characters_to_list(File),
	parse({str, Content}).

start_shell() ->
	io:fwrite("Welcome to the AQL Shell.~n"),
	Line = io:get_line("AQL>"),
	Result = aqlparser:parse({str, Line}),
	start_shell().

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
	Result = select:exec(Table, Select),
	io:fwrite("~p~n", [Result]).

get_table_from_query(Props) ->
	TableName = table:name(Props),
	case table:get_table(TableName) of
		{false, none} ->
			{err, "The table does not exist.~n"};
		Table ->
			Table
	end.
