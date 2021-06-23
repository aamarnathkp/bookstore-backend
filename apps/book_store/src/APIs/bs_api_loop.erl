-module(bs_api_loop).

-export([init/2,
         info/3,
         terminate/3]).


init(Req, State) ->
    Request = cowboy_req:stream_reply(200, Req),
    {cowboy_loop, Request, State, hibernate}.


%% Once initialized, Cowboy will wait for messages to arrive in the process' mailbox.
%% When a message arrives, Cowboy calls the info/3 function with the message, 
%% the Req object and the handler's state.
info({reply, Body}, Req, State) ->
    cowboy_req:reply(200, #{}, Body, Req),
    {stop, Req, State};
info(eof, Req, State) ->
    {stop, Req, State};
info({event, Data}, Req, State) ->
    cowboy_req:stream_body(Data, nofin, Req),
    {ok, Req, State};
info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

terminate(Reason, _Req, _State) ->
    lager:info("Requested terminated with ~p",[Reason]),
    ok.

