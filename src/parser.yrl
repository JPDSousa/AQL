%%====================================================================
%% Nonterminals
%%====================================================================
Nonterminals
query
%select
select_query
projection
select_fields
%where
where_clauses
where_clause
%insert
insert_query
insert_keys_clause
insert_keys
insert_values_clause
insert_values
%create
create_query
table_metadata
create_keys
attribute
attribute_name
%update
update_query
set_clause
set_assignments
set_assignment
%utils
value
.

%%====================================================================
%% Terminals
%%====================================================================
Terminals
%select
select
wildcard
from
%where
where
%insert
insert
into
values
%create
create
table
table_policy
primary
key
default
check
attribute_type
%update
update
set
%types
atom_value
string
number
%expression
assign
increment
decrement
equality
comparator
conjunctive
%list
sep
start_list
end_list
semi_colon
.

%%====================================================================
%% Rootsymbol
%%====================================================================
Rootsymbol query.

%%====================================================================
%% Rules
%%====================================================================

query ->
	query semi_colon query :
	lists:append('$1', '$3').

query ->
	query semi_colon :
	'$1'.

query ->
    select_query :
    ['$1'].

query ->
    insert_query :
    ['$1'].

query ->
	update_query :
	['$1'].

query ->
	create_query :
	['$1'].

%%--------------------------------------------------------------------
%% select query
%%--------------------------------------------------------------------
select_query ->
    select projection from atom_value :
    {select, [{?PROP_TABLE_NAME, '$4'}, {?PROP_COLUMNS, '$2'}]}.

select_query ->
    select projection from atom_value where where_clauses:
    {select, [{?PROP_TABLE_NAME, '$4'}, {?PROP_COLUMNS, '$2'}, {?WHERE_TOKEN, '$6'}]}.

projection ->
    wildcard :
    '$1'.

projection ->
	select_fields:
	'$1'.

select_fields ->
  select_fields sep atom_value :
	lists:flatten('$1', ['$3']).

select_fields ->
	atom_value :
	['$1'].

%%--------------------------------------------------------------------
%% where clause
%%--------------------------------------------------------------------

where_clauses ->
   where_clause :
   ['$1'].

where_clauses ->
   where_clauses conjunctive where_clause :
   lists:append('$1', '$3').

where_clause ->
	atom_value equality value :
	{'$1', '$2', '$3'}.

%%--------------------------------------------------------------------
%% insert query
%%--------------------------------------------------------------------
insert_query ->
    insert into atom_value insert_keys_clause
    values insert_values_clause :
    {?INSERT_TOKEN, [{?PROP_TABLE_NAME, '$3'}, {?PROP_COLUMNS, '$4'}, {?PROP_VALUES, '$6'}]}.

insert_query ->
    insert into atom_value
    values insert_values_clause :
    {?INSERT_TOKEN, [{?PROP_TABLE_NAME, '$3'}, {?PROP_COLUMNS, ?PARSER_WILDCARD}, {?PROP_VALUES, '$5'}]}.

insert_keys_clause ->
    start_list insert_keys end_list :
    '$2'.

insert_keys ->
	insert_keys sep atom_value :
	lists:flatten(['$1', '$3']).

insert_keys ->
	atom_value sep atom_value :
	['$1', '$3'].

insert_keys ->
	atom_value :
	['$1'].

insert_values_clause ->
    start_list insert_values end_list :
    '$2'.

insert_values ->
	insert_values sep value :
	lists:flatten(['$1', '$3']).

insert_values ->
	value sep value :
	['$1', '$3'].

insert_values ->
	value :
	['$1'].

%%--------------------------------------------------------------------
%% update query
%%--------------------------------------------------------------------

update_query ->
	update atom_value set_clause :
	{?UPDATE_TOKEN, [{?PROP_TABLE_NAME, '$2'}, '$3']}.

update_query ->
	update atom_value set_clause where where_clauses :
	{?UPDATE_TOKEN, [{?PROP_TABLE_NAME, '$2'}, '$3', {?WHERE_TOKEN, '$5'}]}.

set_clause ->
	set set_assignments :
	{?SET_TOKEN, '$2'}.

set_assignments ->
	set_assignments conjunctive set_assignment :
	lists:flatten('$1', ['$3']).

set_assignments ->
	set_assignment conjunctive set_assignment :
	['$1', '$3'].

set_assignments ->
	set_assignment :
	['$1'].

%assignment expression
set_assignment ->
	atom_value assign value :
	{'$1', '$2', '$3'}.

%increment/decrement expression
set_assignment ->
	atom_value increment :
	{'$1', '$2', ?PARSER_NUMBER(1)}.

set_assignment ->
	atom_value increment number :
	{'$1', '$2', '$3'}.

set_assignment ->
	atom_value decrement :
	{'$1', '$2', ?PARSER_NUMBER(1)}.

set_assignment ->
	atom_value decrement number :
	{'$1', '$2', '$3'}.

%%--------------------------------------------------------------------
%% create query
%%--------------------------------------------------------------------
create_query ->
	create table table_metadata :
	{?CREATE_TOKEN, '$3'}.

create_query ->
	create table_policy table table_metadata :
	{create, lists:flatten(['$2'], '$4')}.

table_metadata ->
	atom_value start_list create_keys end_list :
	[{?PROP_TABLE_NAME, '$1'}, {?PROP_COLUMNS, '$3'}].

create_keys ->
	create_keys sep attribute :
	lists:flatten(['$1', '$3']).

create_keys ->
	attribute sep attribute :
	['$1', '$3'].

create_keys ->
	attribute :
	'$1'.

attribute ->
	attribute_name attribute_type primary key :
	{?PROP_ATTR, [?PROP_ATTR_NAME('$1'), '$2', ?PROP_ATTR_CONSTRAINT(?PRIMARY_TOKEN)]}.

attribute ->
	attribute_name attribute_type default value :
	{?PROP_ATTR, [?PROP_ATTR_NAME('$1'), '$2', ?PROP_ATTR_CONSTRAINT(?DEFAULT_KEY('$4'))]}.

attribute ->
	attribute_name attribute_type check comparator value :
	{?PROP_ATTR, [?PROP_ATTR_NAME('$1'), '$2', ?PROP_ATTR_CONSTRAINT({'$4', '$5'})]}.

attribute ->
	attribute_name attribute_type :
	{?PROP_ATTR, [?PROP_ATTR_NAME('$1'), '$2', ?PROP_ATTR_CONSTRAINT(?NO_CONSTRAINT)]}.

attribute_name ->
	atom_value :
	'$1'.

%%--------------------------------------------------------------------
%% utils
%%--------------------------------------------------------------------
value ->
	number :
	'$1'.

value ->
	string :
	'$1'.

%%====================================================================
%% Erlang Code
%%====================================================================
Erlang code.

-include("parser.hrl").
