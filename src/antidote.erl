%% @author joao
%% @doc antidote's native API wrapper.


-module(antidote).

-include("specs.hrl").

-type key() :: atom().
-type bucket() :: atom().
-type bound_object() :: {key(), crdt_type(), bucket()}.
-type bound_objects() :: [bound_object()] | bound_object().
-type vectorclock() :: term(). % check antidote project
-type snapshot_time() :: vectorclock() | ignore.
-type ref() :: {node_ref(), txid()}.
-type txid() :: term(). % check antidote project
-type node_ref() :: term().
-type properties() :: term() | [].

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_transaction/1, start_transaction/3,
				read_objects/2,
				commit_transaction/1,
		 		update_objects/2]).

-export([handleBadRpc/1]).

-spec start_transaction(node_ref()) -> {ok, ref()} | error().
start_transaction(Node) ->
	start_transaction(Node, ignore, []).

-spec start_transaction(node_ref(), snapshot_time(), properties()) -> {ok, ref()} | error().
start_transaction(Node, Snapshot, Props) ->
	case call(Node, start_transaction, [Snapshot, Props]) of
		{ok, TxId} ->
			{ok, {Node, TxId}};
		Else ->
			Else
	end.

-spec commit_transaction(ref()) -> {ok, vectorclock()} | error().
commit_transaction({Node, TxId}) ->
	Res = call(Node, commit_transaction, [TxId]),
	Res.

-spec read_objects(bound_objects(), ref()) -> {ok, [term()]} | error().
read_objects(Objects, {Node, TxId}) when is_list(Objects) ->
	call(Node, read_objects, [Objects, TxId]);
read_objects(Object, Ref) ->
	read_objects([Object], Ref).

-spec update_objects(bound_objects(), ref()) -> ok | error().
update_objects(Objects, {Node, TxId}) when is_list(Objects) ->
	call(Node, update_objects, [Objects, TxId]);
update_objects(Object, Ref) ->
	update_objects([Object], Ref).

-spec handleBadRpc(no_permissions()) -> no_permissions_msg().
handleBadRpc({'EXIT', {{{badmatch, {error, no_permissions}}, _}}}) ->
	{"Constraint Breach", "A numeric invariant has been breached."};
handleBadRpc(_Msg) ->
	{"Internal Error", "Unexpected error"}.


%% ====================================================================
%% Internal functions
%% ====================================================================

call(Node, Function, Args) ->
	rpc:call(Node, antidote, Function, Args).
