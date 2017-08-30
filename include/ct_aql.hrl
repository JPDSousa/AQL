
-define(value(Key, Config), proplists:get_value(Key, Config)).
-define(format(Key, Values, Config), lists:flatten(io_lib:format(?value(Key, Config), Values))).