%%%-------------------------------------------------------------------
%% @doc aql top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(aql_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Cache = ?CHILD(cache, worker, [table_cache, [{n, 10}, {ttl, 60}]]),
    {ok, { {one_for_all, 0, 1}, 
        [Cache,
         {console, 
             {aqlparser, start, []}, permanent, 5000, worker, [aqlparser]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
