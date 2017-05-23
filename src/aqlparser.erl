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
	{ok, _CT} = tables:write_table(Table);
exec({?INSERT_TOKEN, Insert}) ->
	{ok, Table} = get_table_from_query(Insert),
	ok = insert:exec(Table, Insert);
exec({?UPDATE_TOKEN, Update}) ->
	{ok, Table} = get_table_from_query(Update),
	ok = update:exec(Table, Update);
exec({?SELECT_TOKEN, Select}) ->
	{ok, Table} = get_table_from_query(Select),
	Result = select:exec(Table, Select),
	io:fwrite("~p~n", [Result]).

get_table_from_query(Props) ->
	{ok, TableName} = query_utils:table_name(Props),
	case tables:get_table(TableName) of
		{true, Table} ->
			{ok, Table};
		_Else ->
			{err, "The table does not exist.~n"}
	end.
