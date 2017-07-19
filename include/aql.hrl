-define(METADATA_BUCKET, aql_metadata).

% AQL -> CRDT mappings
-define(AQL_INTEGER, integer).
-define(CRDT_INTEGER, antidote_crdt_integer).

-define(AQL_VARCHAR, varchar).
-define(CRDT_VARCHAR, antidote_crdt_lwwreg).

-define(AQL_BOOLEAN, boolean).
-define(CRDT_BOOLEAN, antidote_crdt_flag_ew).

-define(AQL_COUNTER_INT, counter_int).
-define(CRDT_COUNTER_INT, antidote_crdt_bcounter).

-define(CRP_LWW, lww).

% custom guards
-define(is_table(Table), is_list(Table)).
-define(is_tname(Name), is_atom(Name)).

-define(is_column(Column), is_list(Column)).
-define(is_cname(Name), is_atom(Name)).

-define(is_dbkey(Key), is_atom(Key)).
-define(is_crdt(CRDT), is_atom(CRDT)).
-define(is_dbbucket(Bucket), is_atom(Bucket)).


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
