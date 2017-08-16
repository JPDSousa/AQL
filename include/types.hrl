
-define(T_TABLE(Name, Policy, Cols, FKeys), {Name, Policy, Cols, FKeys}).
-define(is_table(Table), is_tuple(Table) andalso tuple_size(Table) =:= 4).

-define(T_ELEMENT(BObj, Table, Ops, Data), {BObj, Table, Ops, Data}).
-define(is_element(Element), is_tuple(Element) andalso tuple_size(Element) =:= 4).

-define(T_COL(Name, Type, Constraint), {Name, Type, Constraint}).
-define(is_column(Column), is_tuple(Column) andalso tuple_size(Column) =:= 3).

-define(T_FK(SName, SType, TTName, TName), {{SName, SType}, {TTName, TName}}).
-define(is_fk(Fk), is_tuple(Fk) andalso tuple_size(Fk) =:= 2).
