% -----------------------------------------------------------------------------
% --------------------------------- Lexer ------------------------------------
% -----------------------------------------------------------------------------

% select
-define (SELECT_TOKEN, select).
-define(SELECT_CLAUSE(TokenChars), {?SELECT_TOKEN, TokenChars}).
%% from
-define(FROM_TOKEN, from).
-define(FROM_CLAUSE(TokenChars), {?FROM_TOKEN, TokenChars}).

% where
-define(WHERE_TOKEN, where).
-define(WHERE_CLAUSE(TokenChars), {?WHERE_TOKEN, TokenChars}).
%% and
-define(CONJUNCTIVE_TOKEN, conjunctive).
-define(CONJUNCTIVE_KEY(TokenChars), {?CONJUNCTIVE_TOKEN, TokenChars}).

% insert
-define(INSERT_TOKEN, insert).
-define(INSERT_CLAUSE(TokenChars), {?INSERT_TOKEN, TokenChars}).
%% into
-define(INTO_TOKEN, into).
-define(INTO_KEY(TokenChars), {?INTO_TOKEN, TokenChars}).

% create
-define(CREATE_TOKEN, create).
-define(CREATE_CLAUSE(TokenChars), {?CREATE_TOKEN, TokenChars}).
%% table
-define(TABLE_TOKEN, table).
-define(TABLE_KEY(TokenChars), {?TABLE_TOKEN, TokenChars}).
%% values
-define(VALUES_TOKEN, values).
-define(VALUES_CLAUSE(TokenChars), {?VALUES_TOKEN, TokenChars}).
%% primary key constraint
-define(PRIMARY_TOKEN, primary).
-define(PRIMARY_KEY(TokenChars), {?PRIMARY_TOKEN, TokenChars}).
-define(FOREIGN_TOKEN, foreign).
-define(FOREIGN_KEY(TokenChars), {?FOREIGN_TOKEN, TokenChars}).
-define(KEY_TOKEN, key).
-define(KEY_KEY(TokenChars), {?KEY_TOKEN, TokenChars}).
-define(REFERENCES_TOKEN, references).
-define(REFERENCES_KEY(TokenChars), {?REFERENCES_TOKEN, TokenChars}).
%% check constraint
-define(CHECK_TOKEN, check).
-define(CHECK_KEY(TokenChars), {?CHECK_TOKEN, TokenChars}).
-define(COMPARATOR_KEY(Comparator), {comparator, Comparator}).
-define(GREATER_TOKEN, greater).
-define(GREATER_KEY, ?COMPARATOR_KEY(?GREATER_TOKEN)).
-define(SMALLER_TOKEN, smaller).
-define(SMALLER_KEY, ?COMPARATOR_KEY(?SMALLER_TOKEN)).
%% default value
-define(DEFAULT_TOKEN, default).
-define(DEFAULT_KEY(TokenChars), {?DEFAULT_TOKEN, TokenChars}).
%% attributes
-define(ATTR_TYPE_TOKEN, attribute_type).
-define(ATTR_KEY(AttrType), {?ATTR_TYPE_TOKEN, AttrType}).
%% table policies
-define(TABLE_POLICY_TOKEN, table_policy).
-define(TABLE_POLICY_KEY(Crp), {?TABLE_POLICY_TOKEN, Crp}).

% udpate
-define(UPDATE_TOKEN, update).
-define(UPDATE_CLAUSE(TokenChars), {?UPDATE_TOKEN, TokenChars}).
%% set
-define(SET_TOKEN, set).
-define(SET_CLAUSE(TokenChars), {?SET_TOKEN, TokenChars}).
%%% set ops
-define(SET_OP_T(Token, TChars), {Token, TChars}).
-define(ASSIGN_TOKEN, assign).
-define(ASSIGN_OP(TokenChars), ?SET_OP_T(?ASSIGN_TOKEN, TokenChars)).
-define(INCREMENT_TOKEN, increment).
-define(INCREMENT_OP(TokenChars), ?SET_OP_T(?INCREMENT_TOKEN, TokenChars)).
-define(DECREMENT_TOKEN, decrement).
-define(DECREMENT_OP(TokenChars), ?SET_OP_T(?DECREMENT_TOKEN, TokenChars)).

% delete
-define(DELETE_TOKEN, delete).
-define(DELETE_CLAUSE(TokenChars), {?DELETE_TOKEN, TokenChars}).

%terms
-define(PARSER_ATOM_TOKEN, atom_value).
-define(PARSER_STRING_TOKEN, string).
-define(PARSER_NUMBER_TOKEN, number).
-define(PARSER_TYPE(Type, Value), {Type, Value}).
-define(PARSER_ATOM(Atom), ?PARSER_TYPE(?PARSER_ATOM_TOKEN, Atom)).
-define(PARSER_STRING(String), ?PARSER_TYPE(?PARSER_STRING_TOKEN, String)).
-define(PARSER_NUMBER(Number), ?PARSER_TYPE(?PARSER_NUMBER_TOKEN, Number)).
-define(is_parser(Parser), is_tuple(Parser) andalso tuple_size(Parser) =:= 2).
-define(is_parser_type(Parser, Type), ?is_parser(Parser) andalso element(1, Parser) =:= Type).

% extras
-define(PARSER_EQUALITY, {equality, ignore}).
-define(PARSER_WILDCARD, {wildcard, ignore}).

-define(PARSER_SLIST, {start_list, ignore}).
-define(PARSER_ELIST, {end_list, ignore}).
-define(PARSER_SEP, {sep, ignore}).
-define(PARSER_SCOLON, {semi_colon, ignore}).

% -----------------------------------------------------------------------------
% --------------------------------- Parser ------------------------------------
% -----------------------------------------------------------------------------

% table
-define(PROP_TABLE_NAME, table_name).
-define(PROP_COLUMNS, columns).

% insert
-define(PROP_VALUES, values).

% create
-define(PROP_ATTR, attribute).
-define(PROP_ATTR_NAME(Name), {name, Name}).
-define(PROP_ATTR_CONSTRAINT(Constraint), {constraint, Constraint}).
-define(NO_CONSTRAINT, none).
