%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-04-30 11:45:39.975160
%%%-------------------------------------------------------------------

-module(bs_api_routes).


-export([api_routes/1]).

%%%===================================================================
%%% API
%%%===================================================================

api_routes(#{method := Method} = Args) ->
  api_routes(Method, Args);
api_routes(_) ->
  {error, "API Method not found"}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

api_routes(<<"GET">>, #{path := Path} = ArgsMap) ->
  lager:info("get path ~p",[Path]),
  handle_get(Path, ArgsMap);
api_routes(<<"PUT">>, #{path := Path} = ArgsMap) ->
  handle_put(Path, ArgsMap);
api_routes(<<"DELETE">>, #{path:= Path} = _ArgsMap) ->
  handle_delete(Path);
api_routes(<<"POST">>, #{path := Path} = ArgsMap) ->
  lager:info("post path ~p",[Path]),
  handle_post(Path, ArgsMap);
api_routes(<<"OPTIONS">>, _) ->
  lager:info("options"),
  ok.


%%%==================================================================
%%% GET Routes
%%%==================================================================

handle_get([<<"authors">>], ArgsMap) ->
  bs_authors:list_all(ArgsMap);
handle_get([<<"books">>], _ArgsMap) ->
  bs_books:list_all();
handle_get([<<"counts">>], _ArgsMap) ->
  bs_dashboard:all_counts();
handle_get(_Path, _ArgsMap) ->
  {error, "invalid get request"}.


%%%==================================================================
%%% PUT Routes
%%%==================================================================

handle_put([<<"Authors">>, AuthorId], ArgsMap) ->
  bs_authors:modify_api(ArgsMap#{uuid => AuthorId});
handle_put(_Path, _ArgsMap) ->
  {error, "invalid get request"}.


%%%==================================================================
%%% DELETE Routes
%%%==================================================================

handle_delete(_Path) ->
  {error, "invalid get request"}.


%%%==================================================================
%%% POST Routes
%%%==================================================================

handle_post(undefined, ArgsMap) ->
 bs_identity:login(ArgsMap);
handle_post([<<"logout">>], ArgsMap) ->
  bs_identity:logout(ArgsMap);
handle_post([<<"add_author">>], ArgsMap) ->
  bs_authors:add_api(ArgsMap);
handle_post([<<"author">>, AuthorId], ArgsMap) ->
  bs_authors:modify_api(ArgsMap#{uuid => AuthorId});
handle_post([<<"add_book">>], ArgsMap) ->
  bs_books:add_api(ArgsMap);
handle_post([<<"book">>, BookId], ArgsMap) ->
  bs_books:modify_api(ArgsMap#{uuid => BookId});
handle_post([<<"upload">>], ArgsMap) ->
  bs_file:upload(ArgsMap);
handle_post(_Path, _ArgsMap) ->
  {error, "invalid post request"}.




