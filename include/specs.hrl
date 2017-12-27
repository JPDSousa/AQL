%% @author JPDSousa
%% @doc types for specs

% general
-type reason() -> term().
-type error() -> {error, reason()}.

% antidote
-type key() :: atom().
-type crdt_type() :: antidote_crdt_bcounter
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


