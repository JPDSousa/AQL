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

exec(?SHOW_CLAUSE(?TABLES_TOKEN)) ->
	{ok, TxId} = antidote:start_transaction(),
	Tables = table:read_tables(TxId),
	TNames = lists:map(fun({{Name, _Type}, _Value}) -> Name end, Tables),
	antidote:commit_transaction(TxId),
	io:fwrite("Tables: ~p~n", [TNames]),
	TNames;
exec(?SHOW_CLAUSE({?INDEX_TOKEN, TName})) ->
	{ok, TxId} = antidote:start_transaction(),
	Keys = index:keys(TName, TxId),
	lists:foreach(fun({Key, _Type, _TName}) ->
		io:fwrite("{key: ~p, table: ~p}~n", [Key, TName])
	end, Keys),
	antidote:commit_transaction(TxId),
	Keys;
exec(?CREATE_CLAUSE(Table)) ->
	eval("Create Table", Table, table);
exec(?INSERT_CLAUSE(Insert)) ->
	eval("Insert", Insert, insert);
exec(?DELETE_CLAUSE(Delete)) ->
	eval("Delete", Delete, delete);
exec({?UPDATE_TOKEN, Update}) ->
	eval("Update", Update, update);
exec({?SELECT_TOKEN, Select}) ->
	eval("Select", Select, select);
exec(_Invalid) ->
	throw("Invalid query").

eval(QName, Props, M) ->
	{ok, TxId} = antidote:start_transaction(),
	case M of
		table ->
			Status = M:exec(Props, TxId);
		_Else ->
			Tables = get_table_from_query(M, Props, TxId),
			Status = M:exec(Tables, Props, TxId)
	end,
	antidote:commit_transaction(TxId),
	eval_status(QName, Status).

eval_status(Query, Status) ->
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
		{badrpc, Msg} ->
			{Error, Desc} = antidote:handleBadRpc(Msg),
			io:fwrite("[Err] ~p: ~p~n", [Error, Desc]),
			throw(Desc);
		Msg ->
			io:fwrite("[????] ~p: ~p~n", [AQuery, Msg]),
			Msg
	end.

get_table_from_query(M, Props, TxId) ->
	TableName = M:table(Props),
	Tables = table:read_tables(TxId),
	Table = table:lookup(TableName, Tables),
	{Table, Tables}.
