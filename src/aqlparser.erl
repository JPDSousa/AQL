%%%-------------------------------------------------------------------
%% @doc aqlparser public API
%% @end
%%%-------------------------------------------------------------------

-module(aqlparser).

-type input() :: input_str() | input_file().
-type input_str() :: {str, list()}.
-type input_file() :: {file, term()}.

-type queries() :: [aqlquery()].
-type aqlquery() :: create_query()
									| insert_query()
									| update_query()
									| select_query().

-type create_query() :: {create, create_query_props()}.
-type create_query_props() :: [create_policy() | create_name() | create_keys()].
-type create_name() :: {name, term()}. %incomplete
-type create_policy() :: {table_policy, term()}. %incomplete
-type create_keys() :: {keys, keys_list()}.
-type keys_list() :: term(). %incomplete

-type insert_query() :: {insert, insert_query_props()}.
-type update_query() :: {update, update_query_props()}.
-type select_query() :: {select, select_query_props()}.
-type queryResult() :: term().

-type insert_query_props() :: term().
-type update_query_props() :: term().
-type select_query_props() :: term().

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
exec({create, Table}) ->
	{ok, _CT} = tables:write_table(Table);
exec({insert, Insert}) ->
	{ok, Table} = get_table_from_query(Insert),
	ok = insert:exec(Table, Insert);
exec({update, Update}) ->
	{ok, Table} = get_table_from_query(Update),
	ok = update:exec(Table, Update);
exec({select, Select}) ->
	{ok, Table} = get_table_from_query(Select),
	Result = select:exec(Table, Select),
	io:fwrite("~p~n", [Result]).

get_table_from_query(Props) ->
	{ok, TableName} = query_utils:table_name(Props),
	case tables:get_table(TableName) of
		{true, Table} ->
			{ok, Table};
		_Else ->
			{err, io:fwrite("The table does not exist.")}
	end.
