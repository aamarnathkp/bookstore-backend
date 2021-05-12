%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-05-07 06:55:59.042847
%%%-------------------------------------------------------------------

-module(bs_dashboard).

-include("bs.hrl").

%% API
-export([all_counts/0]).

%%%===================================================================
%%% API
%%%===================================================================

all_counts() ->
  handle_counts().

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_counts() ->
  Books = bs_books:count(),
  Authors = bs_authors:count(),
  Users = bs_users:count(),
  {ok, Users#{books => Books, authors => Authors}}.



