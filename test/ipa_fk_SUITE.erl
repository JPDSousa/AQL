
-module(ipa_fk_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2,
          all/0]).

-export([insert_test/1,
          delete_test/1]).

init_per_suite(Config) ->
  tutils:aql("CREATE TABLE FkB (id INT PRIMARY KEY);"),
  tutils:aql("CREATE TABLE FkA (id INT PRIMARY KEY, b INT FOREIGN KEY REFERENCES B(id));"),
  Config.

end_per_suite(Config) ->
  Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_, _) ->
ok.

all() ->
  [insert_test,
  delete_test].

insert_test(_Config) ->
  {ok, []} = tutils:aql("INSERT INTO FkB VALUES (1)"),
  {ok, [Res]} = tutils:aql("SELECT * FROM FkB WHERE id = 1"),
  ?assertEqual(ipa:new(), element:st_value(Res)).

delete_test(_Config) ->
  {ok, []} = tutils:aql("INSERT INTO FKB VALUES (1)"),
  {ok, []} = tutils:aql("DELETE FROM B WHERE id = 1"),
  {ok, [Res]} = tutils:aql("SELECT * FROM FkB WHERE id = 1"),
  ?assertEqual(ipa:delete(), element:st_value(Res)).
