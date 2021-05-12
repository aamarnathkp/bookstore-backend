%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-04-30 11:45:39.975160
%%%-------------------------------------------------------------------

-module(bs_web).

-behaviour(gen_server).

%% API
-export([start_link/0,
         restart_http/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

restart_http() ->
  gen_server:cast(?MODULE, restart_http).

init(_Args) ->
    start_http(),
   {ok, #{}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(restart_http, State) ->
  stop_http(),
  start_http(),
  {noreply, State};
handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


%============================
%============================

start_http() ->
    DispatchSource = dispatch_source(),
    Port = 7778,
    Dispatch = cowboy_router:compile(DispatchSource),
    cowboy:start_clear(bookstore,
                         [{port, Port}],
                          #{env => #{dispatch => Dispatch},
                            idle_timeout => 300000}).

dispatch_source() ->
  WebDataPath = bs_utils:web_data_path(),
  [{'_', [{"/", cowboy_static, {file, "www/index.html"}},
          {"/images/[...]", cowboy_static, {dir, "var/data/images"}},
          {"/api", bs_api, []},
          {"/api_loop", bs_api_loop, []},
          {"/login", bs_api_rest, #{}},
          {"/guest/[...]", bs_api_rest, #{}},
          {"/api_rest/[...]", bs_api_rest, #{}},
          {"/api_ws", bs_ws, #{}},
          {"/dashboard", bs_dashboard_ws, #{}},
          {"/api/download/[...]",
           cowboy_static,
           {dir, WebDataPath, [{mimetypes, {<<"application">>,
                                            <<"octet-stream">>,
                                            []}
                               }]}},
          {"/[...]", cowboy_static,
                         {dir, WebDataPath, [{mimetypes, cow_mimetypes, all}]}}
  ]}].

stop_http() ->
  cowboy:stop_listener(bookstore).


