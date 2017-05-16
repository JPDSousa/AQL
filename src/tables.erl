%% @author Joao
%% @doc @todo Add description to tables.

-module(tables).

-define(BOUND_OBJECT, {'#tables1', antidote_crdt_gmap, aql_metadata}).
-define(CRDT_TYPE, antidote_crdt_lwwreg).

-export([read_tables/0,
				write_table/1,
				get_table/1, get_table/2,
				name/1]).

-export([primary_key/1,
				collumn_metadata/2,
				key_name/1,
				key_constraint/1,
				key_type/1]).

%% ====================================================================
%% Table functions
%% ====================================================================

read_tables() ->
	{ok, [TablesMeta], _CommitTime} = antidote:read_objects(?BOUND_OBJECT),
	{ok, TablesMeta}.

write_table(Table) ->
	{ok, Name} = tables:name(Table),
	TableUpdate = create_table_update(Name, Table),
	antidote:update_objects(TableUpdate).

create_table_update(Name, Table) ->
	Op = {assign, Table},
	objects:create_single_map_update(?BOUND_OBJECT, Name, ?CRDT_TYPE, Op).

get_table(Name) ->
	{ok, Tables} = read_tables(),
	io:fwrite("~p~n", [Tables]),
	get_table(Tables, Name).

get_table([Table | Tail], Name) ->
	{ok, TName} = name(Table),
	case TName of
		Name ->
			{_Key, Value} = Table,
			{true, Value};
		_Else ->
			get_table(Tail, Name)
	end;
get_table([], _) ->
	{false, none}.

name({{TName, _TType}, _TMeta}) ->
	{ok, TName};
name({table, Table}) ->
	name(Table);
name([{name, {atom_value, Name}} | _]) ->
	{ok, Name};
name([_ | Tail]) ->
	name(Tail);
name([]) ->
	{err, "Cannot resolve table name"}.

%% ====================================================================
%% Key functions
%% ====================================================================

collumn_metadata({{_TName, _TType}, TMeta}, Key) ->
	collumn_metadata(TMeta, Key);
collumn_metadata({table, Table}, Key) ->
	{ok, Keys} = query_utils:search_clause(keys, Table),
	collumn_metadata(Keys, Key);
collumn_metadata([{attribute, KeyData} | Tail], Key) ->
	{ok, Name} = key_name(KeyData),
	if
		Name =:= Key ->
			{ok, KeyData};
		true ->
			collumn_metadata(Tail, Key)
	end;
collumn_metadata([], Key) ->
	{err, lists:concat(["Collumn ", Key, " does not exist"])}.

primary_key({{_TName, _TType}, TMeta}) ->
	primary_key(TMeta);
primary_key({table, Table}) ->
	{ok, Keys} = query_utils:search_clause(keys, Table),
	primary_key(Keys);
primary_key([{attribute, KeyData} | Tail]) ->
	{ok, Constraint} = key_constraint(KeyData),
	if
		Constraint =:= primary_key ->
			key_name(KeyData);
		true ->
			primary_key(Tail)
	end;
primary_key([]) ->
	{err, "Could not find primary key"}.

key_name([{name, {atom_value, Name}} | _]) ->
	{ok, Name};
key_name([_|Tail]) ->
	key_name(Tail);
key_name([]) ->
	{err, "Could not resolve key name"}.

key_constraint([{constraint, Constraint} | _]) ->
	{ok, Constraint};
key_constraint([_|Tail]) ->
	key_constraint(Tail);
key_constraint([]) ->
	{err, "Could not resolve key constraint"}.

key_type([{attribute_type, AqlType} | _]) ->
	{ok, AqlType};
key_type([_|Tail]) ->
	key_type(Tail);
key_type([]) ->
	{err, "Could not resolve key type"}.

%% ====================================================================
%% Internal functions
%% ====================================================================
