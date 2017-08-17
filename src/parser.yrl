%%====================================================================
%% Nonterminals
%%====================================================================
Nonterminals
query
statement
admin
%show
show_query
%select
select_query projection select_fields
%where
where_clauses where_clause
%insert
insert_query insert_keys_clause insert_keys insert_values_clause insert_values
%delete
delete_query
%create
create_query create_keys attribute attribute_constraint
attribute_name
%update
update_query set_clause set_assignments set_assignment
%utils
value atom number_unwrap
.

%%====================================================================
%% Terminals
%%====================================================================
Terminals
%show
show tables
%index
index
%select
select wildcard from
%where
where
%insert
insert into values
%delete
delete
%create
create table table_policy primary foreign key references default check
attribute_type dep_policy
%update
update set
%types
atom_value string number
%expression
assign increment decrement equality comparator conjunctive
%list
sep start_list end_list semi_colon
.

%%====================================================================
%% Rootsymbol
%%====================================================================
Rootsymbol query.

%%====================================================================
%% Rules
%%====================================================================

query -> statement : '$1'.

query -> admin: '$1'.

statement -> statement semi_colon statement :	lists:append('$1', '$3').

statement -> statement semi_colon :	'$1'.

statement -> select_query : ['$1'].

statement -> insert_query : ['$1'].

statement -> delete_query : ['$1'].

statement -> update_query :	['$1'].

statement -> create_query :	['$1'].

admin -> show_query : ['$1'].

%%--------------------------------------------------------------------
%% show
%%--------------------------------------------------------------------
show_query ->
	show index from atom :
	?SHOW_CLAUSE({?INDEX_TOKEN, '$4'}).

show_query ->
	show tables :
	?SHOW_CLAUSE(?TABLES_TOKEN).

%%--------------------------------------------------------------------
%% select query
%%--------------------------------------------------------------------
select_query ->
    select projection from atom :
		?SELECT_CLAUSE({'$4', '$2', ?PARSER_WILDCARD}).

select_query ->
    select projection from atom where where_clauses:
		?SELECT_CLAUSE({'$4', '$2', '$6'}).

projection ->
    wildcard :
    '$1'.

projection ->
	select_fields:
	'$1'.

select_fields ->
  select_fields sep atom :
	lists:append('$1', ['$3']).

select_fields ->
	atom :
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
	atom equality value :
	{'$1', '$2', '$3'}.

%%--------------------------------------------------------------------
%% insert query
%%--------------------------------------------------------------------
insert_query ->
    insert into atom insert_keys_clause
    values insert_values_clause :
		?INSERT_CLAUSE({'$3', '$4', '$6'}).

insert_query ->
    insert into atom
    values insert_values_clause :
		?INSERT_CLAUSE({'$3', ?PARSER_WILDCARD, '$5'}).

insert_keys_clause ->
    start_list insert_keys end_list :
    '$2'.

insert_keys ->
	insert_keys sep atom :
	lists:append('$1', ['$3']).

insert_keys ->
	atom :
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
	update atom set_clause :
	?UPDATE_CLAUSE({'$2', '$3', ?PARSER_WILDCARD}).

update_query ->
	update atom set_clause where where_clauses :
	?UPDATE_CLAUSE({'$2', '$3', '$5'}).

set_clause ->
	set set_assignments :
	?SET_CLAUSE('$2').

set_assignments ->
	set_assignments conjunctive set_assignment :
	lists:append('$1', ['$3']).

set_assignments ->
	set_assignment :
	['$1'].

%assignment expression
set_assignment ->
	atom assign value :
	{'$1', '$2', '$3'}.

%increment/decrement expression
set_assignment ->
	atom increment :
	{'$1', '$2', 1}.

set_assignment ->
	atom increment number_unwrap :
	{'$1', '$2', '$3'}.

set_assignment ->
	atom decrement :
	{'$1', '$2', 1}.

set_assignment ->
	atom decrement number_unwrap :
	{'$1', '$2', '$3'}.

%%--------------------------------------------------------------------
%% create query
%%--------------------------------------------------------------------
create_query ->
	create table_policy table atom start_list create_keys end_list :
	?CREATE_CLAUSE(?T_TABLE('$4', crp:set_table_level(unwrap_type('$2'), crp:new()), '$6', [])).

create_keys ->
	create_keys sep attribute :
	lists:append('$1', ['$3']).

create_keys ->
	attribute :
	['$1'].

attribute ->
	attribute_name attribute_type attribute_constraint :
	?T_COL('$1', unwrap_type('$2'), '$3').

attribute ->
	attribute_name attribute_type :
	?T_COL('$1', unwrap_type('$2'), ?NO_CONSTRAINT).

attribute_constraint ->
	primary key :
	?PRIMARY_TOKEN.

attribute_constraint ->
	foreign key dep_policy references atom start_list atom end_list :
	?FOREIGN_KEY({'$5', '$7', unwrap_type('$3')}).

attribute_constraint ->
	default value :
	?DEFAULT_KEY('$2').

attribute_constraint ->
	check comparator number_unwrap :
	?CHECK_KEY({'$2', '$3'}).

attribute_name ->
	atom :
	'$1'.

%%--------------------------------------------------------------------
%% delete
%%--------------------------------------------------------------------

delete_query ->
	delete from atom :
	?DELETE_CLAUSE({'$3', ?PARSER_WILDCARD}).

delete_query ->
	delete from atom where where_clauses :
	?DELETE_CLAUSE({'$3', '$5'}).

%%--------------------------------------------------------------------
%% utils
%%--------------------------------------------------------------------
atom ->
    atom_value :
    unwrap_type('$1').

number_unwrap ->
    number :
    unwrap_type('$1').

value ->
	number :
	unwrap_type('$1').

value ->
	string :
	unwrap_type('$1').

%%====================================================================
%% Erlang Code
%%====================================================================
Erlang code.

-include("parser.hrl").
-include("types.hrl").

unwrap_type(?PARSER_TYPE(_Type, Value)) -> Value.

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

show_tables_test() ->
	test_parser("SHOW TABLES").

show_index_test() ->
	test_parser("SHOW INDEX FROM TestTable").

create_table_simple_test() ->
	test_parser("CREATE @AW TABLE Test (a VARCHAR, b INTEGER)"),
	test_parser("CREATE @AW TABLE Test (a VARCHAR)"),
	test_parser("CREATE @AW TABLE TestA (a VARCHAR);CREATE @AW TABLE TestB (b INTEGER)").

create_table_pk_test() ->
	test_parser("CREATE @AW TABLE Test (a VARCHAR PRIMARY KEY, b INTEGER)").

create_table_def_test() ->
	test_parser("CREATE @AW TABLE Test (a VARCHAR, b INTEGER DEFAULT 5)").

create_table_check_test() ->
	test_parser("CREATE @AW TABLE Test(a INTEGER, b COUNTER_INT CHECK GREATER 0)").

create_table_fk_test() ->
	test_parser("CREATE @AW TABLE Test (a VARCHAR, b INTEGER FOREIGN KEY @FR REFERENCES TestB(b))").

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

delete_simple_test() ->
	test_parser("DELETE FROM Test"),
	test_parser("DELETE FROM Test WHERE id = 5").

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
