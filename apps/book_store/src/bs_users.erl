%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-05-07 06:52:27.463682
%%%-------------------------------------------------------------------

-module(bs_users).

-include("bs.hrl").

%% API
-export([]).

-export([count/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Utilities
%%%===================================================================

count() ->
 handle_count().

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_count() ->
  Admins = length(mnesia:dirty_index_read(users, admin, role)),
  Users = length(mnesia:dirty_index_read(users, user, role)),
  #{admins => Admins, users => Users}.

