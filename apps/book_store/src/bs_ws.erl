%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-05-12 14:23:28.684343
%%%-------------------------------------------------------------------

-module(bs_ws).

-export([init/2,
        websocket_init/1,
        websocket_handle/2,
        websocket_info/2,
        terminate/3]).


init(Req0, State) ->
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
        undefined ->
            {cowboy_websocket, Req0, State,
                            #{idle_timeout => 30000}};
        Subprotocols ->
            lager:info("Subprotocols ~p",[Subprotocols]),
            case lists:keymember(<<"mqtt">>, 1, Subprotocols) of
                true ->
                    Req = cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                        <<"mqtt">>, Req0),
                    {cowboy_websocket, Req, State};
                false ->
                    Req = cowboy_req:reply(400, Req0),
                    {ok, Req, State}
            end
    end.


%websocket_init(State) ->
%    erlang:start_timer(1000, self(), <<"Hello!">>),
%    {ok, State}.

websocket_init(State) ->
    {reply, {text, <<"WS Connection Established!">>}, State}.

% Cowboy will call websocket_handle/2 whenever a text, binary, ping or pong
% frame arrives from the client.
websocket_handle(Frame = {text, _}, State) ->
    lager:info("WS_handle Frames Received ~p",[Frame]),
    {reply, Frame, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

% Cowboy will call websocket_info/2 whenever an Erlang message arrives.
websocket_info({log, Text}, State) ->
    lager:info("WS_info ~p",[Text]),
    {reply, {text, Text}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _Req, _State) ->
    lager:info("Requested terminated with ~p",[Reason]),
    ok.

