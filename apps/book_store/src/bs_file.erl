%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-05-09 15:49:58.582823
%%%-------------------------------------------------------------------

-module(bs_file).


%% API
-export([upload/1]).


%%%===================================================================
%%% API
%%%===================================================================

upload(Request) ->
  handle_upload(Request).

%%%===================================================================
%%% Internal functions
%%%===================================================================


handle_upload(#{file_data := FileData}) ->
  lager:info("upload data == ~p ",[FileData]),
  UploadPath = bs_utils:bs_image_path('test.png'),
  ok = file:write_file(UploadPath, FileData),
  ok;
handle_upload(Request) ->
  lager:info("Upload Request ~p",[Request]),
  {error, "Invalid upload"}.


