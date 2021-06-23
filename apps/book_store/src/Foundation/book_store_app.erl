%%%-------------------------------------------------------------------
%% @doc book_store public API
%% @end
%%%-------------------------------------------------------------------

-module(book_store_app).

-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    reset_mnesia(),
    reset_lager(),
    book_store_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

reset_mnesia() ->
  application:stop(mnesia),
  BSHome = bs_utils:get_bs_home(),
  MnesiaDir = case application:get_env(mnesia, dir) of
                 undefined ->
                   BSHome ++ "/" ++ "var/mnesia";
                 {ok, MnesiaSysconf} ->
                   BSHome ++ "/" ++ MnesiaSysconf
              end,
  application:set_env(mnesia, dir, MnesiaDir),
  mnesia:create_schema([node()]),
  application:start(mnesia).

reset_lager() ->
  application:stop(lager),
  BSHome = bs_utils:get_bs_home(),
  LagerDir = case application:get_env(lager, log_root) of
                 undefined ->
                   BSHome ++ "/" ++ "var/log/bs";
                 {ok, LagerDirConf} ->
                   BSHome ++ "/" ++ LagerDirConf
              end,
  application:set_env(lager, log_root, LagerDir),
  application:start(lager).

