-define(METADATA_BUCKET, aql_metadata).
-define(MAP_KEY(Key, Type), {Key, Type}).
-define(BOUND_OBJECT(Key, Crdt, Bucket), {Key, Crdt, Bucket}).

% column
-define(C_NAMES, {names}).
-define(C_PK, {pk}).

% AQL -> CRDT mappings
-define(AQL_INTEGER, integer).
-define(CRDT_INTEGER, antidote_crdt_integer).

-define(AQL_VARCHAR, varchar).
-define(CRDT_VARCHAR, antidote_crdt_lwwreg).

-define(AQL_BOOLEAN, boolean).
-define(CRDT_BOOLEAN, antidote_crdt_flag_ew).

-define(AQL_COUNTER_INT, counter_int).
-define(CRDT_COUNTER_INT, antidote_crdt_bcounter).

-define(CRP_AW, aw).
-define(CRP_RW, rw).
-define(CRP_R, r).
-define(CRP_NR, nr).

% types
-export_type([]).%complete with private types

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

-type insert_query_props() :: term().%incomplete
-type update_query_props() :: term().%incomplete
-type select_query_props() :: term().%incomplete
