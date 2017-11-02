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
					try exec(ParseTree, [], Node) of
						Ok -> Ok
					catch
						Reason ->
							io:fwrite("Syntax Error: ~p~n", [Reason]),
							{error, Reason}
					end;
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
	parse({str, Line}, 'antidote@127.0.0.1'),
	read_and_exec().

%%====================================================================
%% Internal functions
%%====================================================================

exec([Query | Tail], Acc, Node) ->
	Res = exec(Query, Node),
	case Res of
		ok ->
			exec(Tail, Acc, Node);
		{ok, NewNode} ->
			exec(Tail, Acc, NewNode);
		_Else ->
			exec(Tail, lists:append(Acc, [Res]), Node)
	end;
exec([], Acc, _Node) ->
	{ok, Acc}.

commit_transaction(Res, Tx) ->
	CommitRes = antidote:commit_transaction(Tx),
	case CommitRes of
		{ok, _CT} ->
			Res;
		_Else ->
			{error, CommitRes}
	end.

exec(Query, Node) when is_atom(Node) ->
	{ok, Tx} = antidote:start_transaction(Node),
	Res = exec(Query, Tx),
	case Res of
		{error, _} ->
			Res;
		_Else ->
			commit_transaction(Res, Tx)
	end;
exec(?SHOW_CLAUSE(?TABLES_TOKEN), Tx) ->
	Tables = table:read_tables(Tx),
	TNames = lists:map(fun({{Name, _Type}, _Value}) -> Name end, Tables),
	io:fwrite("Tables: ~p~n", [TNames]),
	TNames;
exec(?SHOW_CLAUSE({?INDEX_TOKEN, TName}), Tx) ->
	Keys = index:keys(TName, Tx),
	lists:foreach(fun({Key, _Type, _TName}) ->
		io:fwrite("{key: ~p, table: ~p}~n", [Key, TName])
	end, Keys),
	Keys;
exec(?CREATE_CLAUSE(Table), Tx) ->
	eval("Create Table", Table, table, Tx);
exec(?INSERT_CLAUSE(Insert), Tx) ->
	eval("Insert", Insert, insert, Tx);
exec(?DELETE_CLAUSE(Delete), Tx) ->
	eval("Delete", Delete, delete, Tx);
exec({?UPDATE_TOKEN, Update}, Tx) ->
	eval("Update", Update, update, Tx);
exec({?SELECT_TOKEN, Select}, Tx) ->
	eval("Select", Select, select, Tx);
exec(_Invalid, _Node) ->
	throw("Invalid query").

eval(QName, Props, M, Tx) ->
	case M of
		table ->
			Status = M:exec(Props, Tx);
		_Else ->
			Tables = get_table_from_query(M, Props, Tx),
			Status = M:exec(Tables, Props, Tx)
	end,
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
		error ->
			io:fwrite("[Err] ~p~n", [AQuery]),
			{error, Query};
		{error, Msg} ->
			io:fwrite("[Err] ~p: ~p~n", [AQuery, Msg]),
			{error, Msg};
		{badrpc, Msg} ->
			{Error, Desc} = antidote:handleBadRpc(Msg),
			io:fwrite("[Err] ~p: ~p~n", [Error, Desc]),
			{error, Desc};
		Msg ->
			io:fwrite("[????] ~p: ~p~n", [AQuery, Msg]),
			Msg
	end.

get_table_from_query(M, Props, TxId) ->
	TableName = M:table(Props),
	Tables = table:read_tables(TxId),
	Table = table:lookup(TableName, Tables),
	{Table, Tables}.