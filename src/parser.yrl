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
attribute_constraint
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
foreign
key
references
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
	lists:append('$1', ['$3']).

select_fields ->
	atom_value :
	['$1'].

%%--------------------------------------------------------------------
%% where clause
%%--------------------------------------------------------------------

where_clauses ->
   where_clauses conjunctive where_clause :
   lists:append('$1', ['$3']).

 where_clauses ->
	 where_clause :
	 ['$1'].

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
	lists:append('$1', ['$3']).

insert_keys ->
	atom_value :
	['$1'].

insert_values_clause ->
  start_list insert_values end_list :
  '$2'.

insert_values ->
	insert_values sep value :
	lists:append('$1', ['$3']).

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
	lists:append('$1', ['$3']).

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
	{create, lists:append(['$2'], '$4')}.

table_metadata ->
	atom_value start_list create_keys end_list :
	[{?PROP_TABLE_NAME, '$1'}, {?PROP_COLUMNS, '$3'}].

create_keys ->
	create_keys sep attribute :
	lists:append('$1', ['$3']).

create_keys ->
	attribute :
	['$1'].

attribute ->
	attribute_name attribute_type attribute_constraint :
	{?PROP_ATTR, [?PROP_ATTR_NAME('$1'), '$2', ?PROP_ATTR_CONSTRAINT('$3')]}.

attribute ->
	attribute_name attribute_type :
	{?PROP_ATTR, [?PROP_ATTR_NAME('$1'), '$2', ?PROP_ATTR_CONSTRAINT(?NO_CONSTRAINT)]}.

attribute_constraint ->
	primary key :
	?PRIMARY_TOKEN.

attribute_constraint ->
	foreign key references atom_value start_list atom_value end_list :
	?FOREIGN_KEY({'$4', '$6'}).

attribute_constraint ->
	default value :
	?DEFAULT_KEY('$2').

attribute_constraint ->
	check comparator value :
	{'$2', '$3'}.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%====================================================================
%% Eunit tests
%%====================================================================
-ifdef(TEST).

test_parser(String) ->
	{ok, Tokens, _} = scanner:string(String),
	{ok, _ParserTree} = parse(Tokens).

create_table_simple_test() ->
	test_parser("CREATE TABLE Test (a VARCHAR, b INTEGER)"),
	test_parser("CREATE TABLE Test (a VARCHAR)"),
	test_parser("CREATE TABLE TestA (a VARCHAR);CREATE TABLE TestB (b INTEGER)").

create_table_pk_test() ->
	test_parser("CREATE TABLE Test (a VARCHAR PRIMARY KEY, b INTEGER)").

create_table_def_test() ->
	test_parser("CREATE TABLE Test (a VARCHAR, b INTEGER DEFAULT 5)").

create_table_check_test() ->
	test_parser("CREATE TABLE Test(a INTEGER, b COUNTER_INT CHECK GREATER 0)").

create_table_fk_test() ->
	test_parser("CREATE TABLE Test (a VARCHAR, b INTEGER FOREIGN KEY REFERENCES TestB(b))").

update_simple_test() ->
	test_parser("UPDATE Test SET name ASSIGN 'aaa'"),
	test_parser("UPDATE Test SET name ASSIGN 'a';UPDATE Test SET name ASSIGN 'b'").

update_multiple_test() ->
	test_parser("UPDATE Test SET name ASSIGN 'aaa' AND age INCREMENT 3"),
	test_parser("UPDATE Test SET name ASSIGN 'aaa' AND age INCREMENT 3 AND loc ASSIGN 'en' WHERE loc = 'pt'").

update_assign_test() ->
	test_parser("UPDATE Test SET name ASSIGN 'aaa' WHERE name = 'a'"),
	test_parser("UPDATE Test SET age ASSIGN 50 WHERE name = 'aa'").

update_increment_test() ->
	test_parser("UPDATE Test SET countCars INCREMENT WHERE model = 'b'"),
	test_parser("UPDATE Test SET countCars INCREMENT 2 WHERE model = 'b'").

update_decrement_test() ->
	test_parser("UPDATE Test SET countCars DECREMENT WHERE model = 'b'"),
	test_parser("UPDATE Test SET countCars DECREMENT 2 WHERE model = 'b'").

insert_simple_test() ->
	test_parser("INSERT INTO Test VALUES ('a', 5, 'b')"),
	test_parser("INSERT INTO Test (a, b, c) VALUES ('a', 5, 'b')"),
	test_parser("INSERT INTO Test VALUES ('a')"),
	test_parser("INSERT INTO Test (a) VALUES (5)").

select_simple_test() ->
	test_parser("SELECT * FROM Test"),
	test_parser("SELECT a, b FROM Test WHERE c = 5").

select_projection_test() ->
	test_parser("SELECT a FROM Test"),
	test_parser("SELECT a, b FROM Test").

select_where_test() ->
	test_parser("SELECT a FROM Test WHERE b =2"),
	test_parser("SELECT a FROM Test WHERE b = 2 AND c =3 AND d= 4").

-endif.
