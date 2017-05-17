%%====================================================================
%% Nonterminals
%%====================================================================
Nonterminals
query
%select
select_query
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
%delete
delete_query
%create
create_query
table_metadata
create_keys
attribute
attribute_name
%drop
drop_query
%update
update_query
set_query
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
%delete
delete
%drop
drop
%create
create
table
table_policy
primary
key
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
comparators
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
    delete_query :
    ['$1'].

query ->
	update_query :
	['$1'].

query ->
	create_query :
	['$1'].

query ->
	drop_query :
	['$1'].

%%--------------------------------------------------------------------
%% select query
%%--------------------------------------------------------------------
select_query ->
    select select_fields from atom_value :
    {select, [{table, '$4'}, {keys, '$2'}]}.

select_query ->
    select select_fields from atom_value where where_clauses:
    {select, [{table, '$4'}, {keys, '$2'}, {where, '$6'}]}.

select_fields ->
    wildcard :
    '$1'.

select_fields ->
    select_fields sep atom_value :
	lists:flatten('$1', '$3').

select_fields ->
	atom_value sep atom_value :
	['$1', '$3'].

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
    {insert, [{table, '$3'}, {keys, '$4'}, {values, '$6'}]}.

insert_query ->
    insert into atom_value
    values insert_values_clause :
    {insert, [{table, '$3'}, {values, '$5'}]}.

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
	update atom_value set_query :
	{update, [{table, '$2'}, '$3']}.

update_query ->
	update atom_value set_query where_clauses :
	{update, [{table, '$2'}, '$3', '$4']}.

set_query ->
	set set_assignments :
	{set, '$2'}.

set_assignments ->
	set_assignments sep set_assignments :
	lists:flatten('$1', '$3').

set_assignments ->
	set_assignment :
	['$1'].

%assignment expression
set_assignment ->
	atom_value assign value :
	{'$1', '$2', '$3'}.

%increment expression
set_assignment ->
	atom_value increment :
	{'$1', '$2', {number, 1}}.

set_assignment ->
	atom_value increment number :
	{'$1', '$2', '$3'}.

set_assignment ->
	atom_value decrement :
	{'$1', '$2', {number , 1}}.

set_assignment ->
	atom_value decrement number :
	{'$1', '$2', '$3'}.

%%--------------------------------------------------------------------
%% delete query
%%--------------------------------------------------------------------
delete_query ->
    delete from atom_value where where_clauses:
    {delete, [{table, '$3'}, {record_time, '$5'}]}.

%%--------------------------------------------------------------------
%% create query
%%--------------------------------------------------------------------
create_query ->
	create table table_metadata :
	{create, {table, '$3'}}.

create_query ->
	create table_policy table table_metadata :
	{create, {table, lists:flatten(['$2'], '$4')}}.

table_metadata ->
	atom_value start_list create_keys end_list :
	[{name, '$1'}, {keys, '$3'}].

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
	{attribute, [{name, '$1'}, '$2', {constraint, primary_key}]}.

attribute ->
	attribute_name attribute_type check comparators value :
	{attribute, [{name, '$1'}, '$2', {constraint, {'$4', '$5'}}]}.

attribute ->
	attribute_name attribute_type :
	{attribute, [{name, '$1'}, '$2', {constraint, none}]}.

attribute_name ->
	atom_value :
	'$1'.

%%--------------------------------------------------------------------
%% drop query
%%--------------------------------------------------------------------
drop_query ->
	drop table atom_value :
	{drop, [{table, '$3'}]}.

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
