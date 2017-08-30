%%%-------------------------------------------------------------------
%% @doc aqlparser public API
%% @end
%%%-------------------------------------------------------------------

-module(aqlparser).

-include("aql.hrl").
-include("parser.hrl").

%% Application callbacks
-export([parse/2, start_shell/0]).

%%====================================================================
%% API
%%====================================================================

-spec parse(input(), node()) -> queryResult() | {err, term()}.
parse({str, Query}, Node) ->
	TokensRes = scanner:string(Query),
	case TokensRes of
		{ok, Tokens, _} ->
			ParseRes = parser:parse(Tokens),
			case ParseRes of
				{ok, ParseTree} ->
					exec(ParseTree, [], Node);
				_Else ->
					ParseRes
			end;
		_Else ->
			TokensRes
	end;
parse({file, Filename}, Node) ->
	{ok, File} = file:read_file(Filename),
	Content = unicode:characters_to_list(File),
	parse({str, Content}, Node).

start_shell() ->
	io:fwrite("Welcome to the AQL Shell.~n"),
	read_and_exec().

read_and_exec() ->
	Line = io:get_line("AQL>"),
	Res = parse({str, Line}, 'antidote@127.0.0.1'),
	io:fwrite(">> ~p~n", [Res]),
	read_and_exec().

%%====================================================================
%% Internal functions
%%====================================================================

exec([Query | Tail], Acc, Node) ->
	Res = exec(Query, Node),
	case Res of
		ok ->
			exec(Tail, Acc, Node);
		Else ->
			exec(Tail, lists:append(Acc, [Else]), Node)
	end;
exec([], Acc, _Node) ->
	{ok, Acc}.

exec(?SHOW_CLAUSE(?TABLES_TOKEN), Node) ->
	{ok, TxId} = antidote:start_transaction(Node),
	Tables = table:read_tables(TxId),
	TNames = lists:map(fun({{Name, _Type}, _Value}) -> Name end, Tables),
	antidote:commit_transaction(TxId),
	io:fwrite("Tables: ~p~n", [TNames]),
	TNames;
exec(?SHOW_CLAUSE({?INDEX_TOKEN, TName}), Node) ->
	{ok, TxId} = antidote:start_transaction(Node),
	Keys = index:keys(TName, TxId),
	lists:foreach(fun({Key, _Type, _TName}) ->
		io:fwrite("{key: ~p, table: ~p}~n", [Key, TName])
	end, Keys),
	antidote:commit_transaction(TxId),
	Keys;
exec(?CREATE_CLAUSE(Table), Node) ->
	eval("Create Table", Table, table, Node);
exec(?INSERT_CLAUSE(Insert), Node) ->
	eval("Insert", Insert, insert, Node);
exec(?DELETE_CLAUSE(Delete), Node) ->
	eval("Delete", Delete, delete, Node);
exec({?UPDATE_TOKEN, Update}, Node) ->
	eval("Update", Update, update, Node);
exec({?SELECT_TOKEN, Select}, Node) ->
	eval("Select", Select, select, Node);
exec(_Invalid, _Node) ->
	throw("Invalid query").

eval(QName, Props, M, Node) ->
	{ok, TxId} = antidote:start_transaction(Node),
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
