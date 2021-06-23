%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-04-30 11:45:39.975160
%%%-------------------------------------------------------------------

-module(bs_mnesia).

-behaviour(gen_server).

-include("bs.hrl").

%% API
-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([write/1,
         read/2,
         delete/2,
         read_all/1]).


-define(SERVER, ?MODULE).

start_link() ->
   gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
   init_schema(),
   init_tables(),
   {ok, #{}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

init_schema() ->
  mnesia:create_schema([node()]).

init_tables() ->
  mnesia:create_table(users, [{attributes, record_info(fields, users)},
                              {disc_copies, [node()]},
                              {index, [username, role, status]}]),
  mnesia:create_table(logins, [{attributes, record_info(fields, logins)},
                               {disc_copies, [node()]},
                               {index, [user_id]}]),
  mnesia:create_table(authors, [{attributes, record_info(fields, authors)},
                                {disc_copies, [node()]},
                                {index, [name, status, created_by]}]),
  mnesia:create_table(books, [{attributes, record_info(fields, books)},
                              {disc_copies, [node()]},
                              {index, [author_uuid, published_year, language, price, in_stock]}]),
  mnesia:create_table(book_reviews, [{attributes, record_info(fields, book_reviews)},
                                     {disc_copies, [node()]},
                                     {index, [author_uuid, book_uuid, user_uuid]}]),
  mnesia:create_table(quiz, [{attributes, record_info(fields, quiz)},
                              {disc_copies, [node()]},
                              {index, [name, duration]}]).




%%========================
%% Mnesia Operations
%%========================

write(Rec) ->
  mnesia:dirty_write(Rec).

read(TableName, Id) ->
  mnesia:dirty_read(TableName, Id).

delete(TableName, Id) ->
  mnesia:dirty_delete(TableName, Id).

read_all(TableName) ->
  case catch(mnesia:dirty_all_keys(TableName)) of
    {'EXIT', _} -> [];
    _ ->
      [mnesia:dirty_read(TableName, Uuid) ||
             Uuid <- mnesia:dirty_all_keys(TableName)]
  end.



