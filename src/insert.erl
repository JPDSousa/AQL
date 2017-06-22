
	Update = crdt:map_update(ParentKey, [StOp, RefOp]),
	antidote:update_objects(Update, TxId).

	Update = crdt:map_update(ParentKey, [StOp, RefOp]),
	antidote:update_objects(Update, TxId).

update_child(ParentKey, TxId) ->
	% update child tree
	Children = get_child(ParentKey, TxId),
	lists:foreach(fun (C) -> update_child(C, TxId) end, Children),
	% update itself
	StKey = element:st_key(),
	StOp = crdt:field_map_op(StKey, crdt:assign_lww(ipa:touch_cascade())),
	antidote:update_objects(crdt:map_update(ParentKey, StOp), TxId).

get_keys(Table, Props) ->
	Clause = query_utils:search_clause(?PROP_COLUMNS, Props),
	case Clause of
		?PARSER_WILDCARD ->
			Keys = table:get_col_names(Table),
			lists:map(fun (V) -> ?PARSER_ATOM(V) end, Keys);
		_Else ->
			Clause
	end.
