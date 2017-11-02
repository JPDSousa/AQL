%% @author joao
%% @doc @todo Add description to antidote.


-module(antidote).

-type key() :: atom().
-type crdt_type() :: antidote_crdt_bcounter % valid antidote_crdt types
									| antidote_crdt_counter
									| antidote_crdt_fat_counter
									| antidote_crdt_gmap
									| antidote_crdt_gset
									| antidote_crdt_integer
									| antidote_crdt_lwwreg
									| antidote_crdt_map_aw
									| antidote_crdt_map_rr
									| antidote_crdt_mvreg
									| antidote_crdt_orset
									| antidote_crdt_rga
									| antidote_crdt_set_rw.
-type bucket() :: atom().
-type bound_object() :: {key(), crdt_type(), bucket()}.
-type bound_objects() :: [bound_object()] | bound_object().
-type vectorclock() :: term(). % check antidote project
-type snapshot_time() :: vectorclock() | ignore.
-type ref() :: {node_ref(), txid()}.
-type txid() :: term(). % check antidote project
-type node_ref() :: term().
-type reason() :: term().
-type properties() :: term() | [].

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_transaction/1, start_transaction/3,
				read_objects/2,
				commit_transaction/1,
		 		update_objects/2]).

-export([handleBadRpc/1]).

-spec start_transaction(node_ref()) -> {ok, ref()} | {error, reason()}.
start_transaction(Node) ->
	start_transaction(Node, ignore, []).

-spec start_transaction(node_ref(), snapshot_time(), properties()) -> {ok, ref()} | {error, reason()}.
start_transaction(Node, Snapshot, Props) ->
	case call(Node, start_transaction, [Snapshot, Props]) of
		{ok, TxId} ->
			{ok, {Node, TxId}};
		Else ->
			Else
	end.

-spec commit_transaction(ref()) -> {ok, vectorclock()} | {error, reason()}.
commit_transaction({Node, TxId}) ->
	Res = call(Node, commit_transaction, [TxId]),
	Res.

-spec read_objects(bound_objects(), ref()) -> {ok, [term()]}.
read_objects(Objects, {Node, TxId}) when is_list(Objects) ->
	call(Node, read_objects, [Objects, TxId]);
read_objects(Object, Ref) ->
	read_objects([Object], Ref).

-spec update_objects(bound_objects(), ref()) -> ok | {error, reason()}.
update_objects(Objects, {Node, TxId}) when is_list(Objects) ->
	call(Node, update_objects, [Objects, TxId]);
update_objects(Object, Ref) ->
	update_objects([Object], Ref).

handleBadRpc({'EXIT', {{{badmatch, {error, no_permissions}}, _}}}) ->
	{"Constraint Breach", "A numeric invariant has been breached."};
handleBadRpc(_Msg) ->
	{"Internal Error", "Unexpected error"}.


%% ====================================================================
%% Internal functions
%% ====================================================================

call(Node, Function, Args) ->
	rpc:call(Node, antidote, Function, Args).
