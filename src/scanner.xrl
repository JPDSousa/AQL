%%====================================================================
%% Definitions
%%====================================================================
Definitions.

WhiteSpace = ([\000-\s]|%.*)
Equality = =
CharValues = [A-Za-z]
WildCard = \*
IntegerValues = [0-9]
StartList = \(
EndList = \)
Sep = ,
SemiColon = ;
String = '([^']*?)'

%%====================================================================
%% Rules
%%====================================================================
Rules.

% select query related tokens
(select|SELECT) : {token, ?SELECT_CLAUSE(TokenChars)}.
(from|FROM) : {token, ?FROM_CLAUSE(TokenChars)}.

% where clause related tokens
(where|WHERE) : {token, ?WHERE_CLAUSE(TokenChars)}.
(and|AND) : {token, ?CONJUNCTIVE_KEY(TokenChars)}.

% insert query related tokens
(insert|INSERT) : {token, ?INSERT_CLAUSE(TokenChars)}.
(into|INTO) : {token, ?INTO_KEY(TokenChars)}.

% create query related tokens
(create|CREATE) : {token, ?CREATE_CLAUSE(TokenChars)}.
(table|TABLE) : {token, ?TABLE_KEY(TokenChars)}.
(values|VALUES) : {token, ?VALUES_CLAUSE(TokenChars)}.

% conflict resolution policies
(lww|LWW) : {token, ?TABLE_POLICY_KEY(?CRP_LWW)}.

% update query related tokens
(update|UPDATE) : {token, ?UPDATE_CLAUSE(TokenChars)}.
(set|SET) : {token, ?SET_CLAUSE(TokenChars)}.

% set operations
(assign|ASSIGN) : {token, ?ASSIGN_OP(TokenChars)}.
(increment|INCREMENT) : {token, ?INCREMENT_OP(TokenChars)}.
(decrement|DECREMENT) : {token, ?DECREMENT_OP(TokenChars)}.

% comparators
(smaller|SMALLER) : {token, ?SMALLER_KEY}.
(greater|GREATER) : {token, ?GREATER_KEY}.

% constraints
(primary|PRIMARY) : {token, ?PRIMARY_KEY(TokenChars)}.
(key|KEY) : {token, ?KEY_KEY(TokenChars)}.
(check|CHECK) : {token, ?CHECK_KEY(TokenChars)}.

% default
(default|DEFAULT) : {token, ?DEFAULT_KEY(TokenChars)}.

% attribute types
(varchar|VARCHAR) : {token, ?ATTR_KEY(?AQL_VARCHAR)}.
(boolean|BOOLEAN) : {token, ?ATTR_KEY(?AQL_BOOLEAN)}.
(int|INT|integer|INTEGER) : {token, ?ATTR_KEY(?AQL_INTEGER)}.
(counter_int|COUNTER_INT) : {token, ?ATTR_KEY(?AQL_COUNTER_INT)}.


{CharValues}+ : A = list_to_atom(TokenChars),
				{token, ?PARSER_ATOM(A)}.

{String} : S = strip_value(TokenChars),
			{token, ?PARSER_STRING(S)}.

{IntegerValues}+ : {N, _} = string:to_integer(TokenChars),
				{token, ?PARSER_NUMBER(N)}.

{Equality} : {token, ?PARSER_EQUALITY}.
{WildCard} : {token, ?PARSER_WILDCARD}.
{WhiteSpace}+ : skip_token.

{StartList} : {token, ?PARSER_SLIST}.
{EndList} : {token, ?PARSER_ELIST}.
{Sep} : {token, ?PARSER_SEP}.
{SemiColon} : {token, ?PARSER_SCOLON}.

%%====================================================================
%% Erlang Code
%%====================================================================
Erlang code.

-include("parser.hrl").
-include("aql.hrl").

strip_value(Value) ->
	string:strip(Value, both, $').
