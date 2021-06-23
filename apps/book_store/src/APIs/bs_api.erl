%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-04-30 11:45:39.975160
%%%-------------------------------------------------------------------

-module(bs_api).

-export([init/2,
         terminate/3]).


init(Req, State) ->
    Request = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>},
                                        <<"Book Store!!">>, Req),
    {ok, Request, State}.

terminate(Reason, _Req, _State) ->
    lager:info("Requested terminated with ~p",[Reason]),
    ok.

