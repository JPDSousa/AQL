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
-type txid() :: term(). % check antidote project
-type reason() :: term().
-type properties() :: term() | [].

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_transaction/0, start_transaction/2,
				read_objects/1, read_objects/2, read_objects/3,
				commit_transaction/1,
		 		update_objects/1, update_objects/2, update_objects/3]).

-export([handleBadRpc/1]).

-spec start_transaction() -> {ok, txid()} | {error, reason()}.
start_transaction() ->
	start_transaction(ignore, []).

-spec start_transaction(snapshot_time(), properties()) -> {ok, txid()} | {error, reason()}.
start_transaction(Snapshot, Props) ->
	call(start_transaction, [Snapshot, Props]).

-spec commit_transaction(txid()) -> {ok, vectorclock()} | {error, reason()}.
commit_transaction(TxId) ->
	call(commit_transaction, [TxId]).

-spec read_objects(bound_objects()) -> {ok, [term()]}.
read_objects(Objects) when is_list(Objects) ->
	read_objects(ignore, [], Objects);
read_objects(Object) ->
	read_objects([Object]).

-spec read_objects(bound_objects(), txid()) -> {ok, [term()]}.
read_objects(Objects, TxId) when is_list(Objects) ->
	call(read_objects, [Objects, TxId]);
read_objects(Object, TxId) ->
	read_objects([Object], TxId).

-spec read_objects(snapshot_time(), properties, bound_objects) -> {ok, term()}.
read_objects(Snapshot, Props, Objects) when is_list(Objects) ->
	call(read_objects, [Snapshot, Props, Objects]);
read_objects(Snapshot, Props, Object) ->
	read_objects(Snapshot, Props, [Object]).

-spec update_objects(bound_objects()) -> ok | {error, reason()}.
update_objects(Objects) when is_list(Objects) ->
	update_objects(ignore, [], Objects);
update_objects(Object) ->
	update_objects([Object]).

-spec update_objects(bound_objects(), txid()) -> ok | {error, reason()}.
update_objects(Objects, TxId) when is_list(Objects) ->
	call(update_objects, [Objects, TxId]);
update_objects(Object, TxId) ->
	update_objects([Object], TxId).

-spec update_objects(snapshot_time(), properties(), bound_objects()) -> ok | {error, reason()}.
update_objects(Snapshot, Props, Objects) when is_list(Objects) ->
	call(update_objects, [Snapshot, Props, Objects]);
update_objects(Snapshot, Props, Object) ->
	update_objects(Snapshot, Props, [Object]).

handleBadRpc({'EXIT', {{{badmatch, {error, no_permissions}}, _}}}) ->
	{"Constraint Breach", "A numeric invariant has been breached."};
handleBadRpc(_Msg) ->
	{"Internal Error", "Unexpected error"}.


%% ====================================================================
%% Internal functions
%% ====================================================================

connect() ->
	antidote_node(nodes()).

antidote_node([]) ->
	net_adm:ping('antidote@127.0.0.1'),
	antidote_node(nodes());
antidote_node(Nodes) ->
	[Node] = Nodes,
	Node.

call(Function, Args) ->
	Node = connect(),
	rpc:call(Node, antidote, Function, Args).
