
-define(T_TABLE(Name, Policy, Cols, _SCols), {Name, Policy, Cols, _SCols}).
-define(is_table(Table), is_tuple(Table) and tuple_size(Table) == 4).

-define(T_ELEMENT(BObj, Table, Ops, Data), {BObj, Table, Ops, Data}).
-define(is_element(Element), is_tuple(Element) and tuple_size(Element) == 4).

-define(T_COL(Name, Type, Constraint), {Name, Type, Constraint}).
-define(is_column(Column), is_tuple(Column) and tuple_size(Column) == 3).

-define(T_FK(SName, SType, TTName, TName), {{SName, SType}, {TTName, TName}}).
-define(is_fk(Fk), is_tuple(Fk) and tuple_size(Fk) == 2).
