%%%-------------------------------------------------------------------
%%% @author joao
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. ago 2017 11:10
%%%-------------------------------------------------------------------
-module(crp).
-author("joao").

-include("aql.hrl").
-include("types.hrl").

%% API
-export([
  new/0,
  table_level/1, set_table_level/2,
  dep_level/1, set_dep_level/2,
  p_dep_level/1, set_p_dep_level/2,
  get_rule/1
]).


new() ->
  ?T_CRP(undefined, undefined, undefined).

table_level(?T_CRP(TableLevel, _, _)) -> TableLevel.

dep_level(?T_CRP(_, DepLevel, _)) -> DepLevel.

p_dep_level(?T_CRP(_, _, PDepLevel)) -> PDepLevel.

set_table_level(Rule, Crp) ->
  set(Rule, table, Crp).

set_dep_level(Rule, Crp) ->
  set(Rule, dep, Crp).

set_p_dep_level(Rule, Crp) ->
  set(Rule, pdep, Crp).

set(Rule, Level, Crp) ->
  ?T_CRP(TableLevel, DepLevel, PDepLevel) = Crp,
  case Level of
    table -> Prev = TableLevel;
    dep -> Prev = DepLevel;
    _Else -> Prev = PDepLevel
  end,
  case {Level, Prev} of
    {table, undefined} -> ?T_CRP(Rule, DepLevel, PDepLevel);
    {dep, undefined} -> ?T_CRP(TableLevel, Rule, PDepLevel);
    {pdep, undefined} -> ?T_CRP(TableLevel, DepLevel, PDepLevel);
    {Level1, Prev1} ->
      throw(lists:concat([Level1, " already set to ", Prev1]))
  end.

get_rule(Crp) ->
  ?T_CRP(TableLevel, DepLevel, PDepLevel) = Crp,
  Rule1 = rule_table_level(TableLevel),
  Rule2 = rule_dep_level(DepLevel, Rule1),
  rule_p_dep_level(PDepLevel, Rule2).

rule_table_level(?ADD_WINS) -> [ipa:delete(), ipa:insert()];
rule_table_level(?REMOVE_WINS) -> [ipa:insert(), ipa:delete()];
rule_table_level(Crp) -> rule_table_level(table_level(Crp)).

rule_dep_level(?ADD_WINS, Rule) ->
  lists:append([ipa:delete_cascade(), ipa:touch_cascade()], Rule);
rule_dep_level(?REMOVE_WINS, Rule) ->
  lists:append([[ipa:touch_cascade()], Rule, [ipa:delete_cascade()]]);
rule_dep_level(Crp, Rule) -> rule_dep_level(dep_level(Crp), Rule).

rule_p_dep_level(?ADD_WINS, Rule) -> lists:append(Rule, [ipa:touch()]);
rule_p_dep_level(?REMOVE_WINS, Rule) -> lists:append([ipa:touch(), Rule]);
rule_p_dep_level(Crp, Rule) -> rule_p_dep_level(p_dep_level(Crp), Rule).

