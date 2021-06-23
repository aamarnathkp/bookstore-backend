%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-05-20 06:01:52.006838
%%%-------------------------------------------------------------------
-module(quiz).

-include("bs.hrl").

%% API
-export([add_api/1,
         list_all/0]).

%%%===================================================================
%%% API
%%%===================================================================

add_api(Request) ->
  handle_add_api(Request).

list_all() ->
  handle_list_all().

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_add_api(#{name:= Name, duration := Duration}=Request) ->
  Uuid = bs_utils:create_uuid(),
  UserId = maps:get(user_id, Request),
  mnesia:dirty_write(#quiz{name = Name,
                           duration = Duration,
                           created_by = UserId}),
  {ok, #{uuid => Uuid}}.


handle_list_all() ->
  Quiz = [ mnesia:dirty_read(quiz, Id) || Id <- mnesia:dirty_all_keys(quiz)],
  list_quiz(Quiz).

list_quiz(Quiz) ->
  Fields = record_info(fields, quiz),
  QuizMap = bs_utils:rec_to_map(Fields, Quiz),
  Total = length(QuizMap),
  {ok, #{total => Total, %bs_utils:to(binary, Total),
         items => QuizMap}}.

