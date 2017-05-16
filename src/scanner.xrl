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
(select|SELECT) : {token, {select, TokenChars}}.
(from|FROM) : {token, {from, TokenChars}}.

% where clause related tokens
(order|ORDER) : {token, {order, TokenChars}}.
(limit|LIMIT) : {token, {limit, TokenChars}}.
(where|WHERE) : {token, {where, TokenChars}}.
(asc|ASC) : {token, {direction, TokenChars}}.
(des|DES) : {token, {direction, TokenChars}}.
(and|AND) : {token, {conjunctive, TokenChars}}.

% insert query related tokens
(insert|INSERT) : {token, {insert, TokenChars}}.
(into|INTO) : {token, {into, TokenChars}}.

% create query related tokens
(create|CREATE) : {token, {create, TokenChars}}.
(table|TABLE) : {token, {table, TokenChars}}.
(values|VALUES) : {token, {values, TokenChars}}.

% update query related tokens
(update|UPDATE) : {token, {update, TokenChars}}.
(set|SET) : {token, {set, TokenChars}}.

% set operations
(assign|ASSIGN) : {token, {assign, TokenChars}}.
(increment|INCREMENT) : {token, {increment, TokenChars}}.
(decrement|DECREMENT) : {token, {decrement, TokenChars}}.

% comparators
(smaller|SMALLER) : {token, {comparators, smaller}}.
(greater|GREATER) : {token, {comparators, greater}}.

% constraints
(primary|PRIMARY) : {token, {primary, TokenChars}}.
(key|KEY) : {token, {key, TokenChars}}.
(check|CHECK) : {token, {check, TokenChars}}.

% attribute types
(varchar|VARCHAR) : {token, {attribute_type, varchar}}.
(boolean|BOOLEAN) : {token, {attribute_type, boolean}}.
(int|INT|integer|INTEGER) : {token, {attribute_type, integer}}.
(counter_int|COUNTER_INT) : {token, {attribute_type, counter_int}}.
%does antidote support double counters

% drop query related tokens
(drop|DROP) : {token, {drop, TokenChars}}.

% delete query related tokens
(delete|DELETE) : {token, {delete, TokenChars}}.

% conflict resolution policies
(lww|LWW) : {token, {table_policy, TokenChars}}.

{CharValues}+ : A = list_to_atom(TokenChars),
				{token, {atom_value, A}}.

{String} : S = strip_value(TokenChars),
			{token, {string, S}}.

{IntegerValues}+ : {N, _} = string:to_integer(TokenChars),
				{token, {number, N}}.

{Equality} : {token, {equality, TokenChars}}.
{WildCard} : {token, {wildcard, TokenChars}}.
{WhiteSpace}+ : skip_token.

{StartList} : {token, {start_list, TokenChars}}.
{EndList} : {token, {end_list, TokenChars}}.
{Sep} : {token, {sep, TokenChars}}.
{SemiColon} : {token, {semi_colon, TokenChars}}.

%%====================================================================
%% Erlang Code
%%====================================================================
Erlang code.

strip_value(Value) ->
	string:strip(Value, both, $').
