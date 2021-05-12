%%%-------------------------------------------------------------------
%% @doc book_store top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(book_store_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(ID), #{id => ID, start => {ID, start_link, []}}).
-define(CHILD_SUP(ID), #{id=>ID, start => {ID, start_link, []}, type => supervisor}).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupervisorFlags = #{strategy => one_for_one,
                      intensity => 25,
                      period => 60},
  ChildSpecs = [?CHILD(bs_web),
                ?CHILD(bs_mnesia)],
  lager:debug("Child specs are ~p and Supervisor flags are ~p",
              [ChildSpecs, SupervisorFlags]),
  {ok, {SupervisorFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
