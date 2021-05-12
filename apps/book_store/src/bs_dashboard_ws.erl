%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-05-11 12:32:47.525484
%%%-------------------------------------------------------------------

-module(bs_dashboard_ws).

%% APs
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).


%%%===================================================================
%%% API
%%%===================================================================


%%%===================================================================
%%% Callbacks
%%%===================================================================
init(Req, State) ->
      {cowboy_websocket, Req, State#{ws_request => Req}, #{idle_timeout => 120000}}.

websocket_init(State) ->
  lager:info("ws state ~p",[State]),
  send_ping(),
  {ok, State}.

websocket_handle(<<"ping">>, State) ->
  lager:debug("got the ping message"),
%  Status = maps:get(status, State, undefined),
%  case Status of
%    connected ->
      {reply, {text, <<"pong">>}, State};
%    _ ->
%      lager:info("ping message when the client is not yet connected"),
%      {stop, State}
%  end;
websocket_handle({text, <<"pong">>}, State) ->
  lager:debug("got the pong message"),
  TRef = send_ping(),
  {ok, State#{time_ref => TRef}};
websocket_handle({text, Message}, State) ->
  lager:info("Received Connect Message:~p", [Message]),
  case handle_message(Message, State) of
    {error, Reason} ->
      lager:error("handling message failed so stopping the client:~p",[Reason]),
      {stop, State};
    {Reply, StateU} ->
      {reply, {text, Reply}, StateU}
  end;
websocket_handle(Data, State) ->
  {reply, Data, State}.

websocket_info(<<"ping">>, State) ->
  TimeRef = maps:get(time_ref, State, undefined),
  if TimeRef == undefined -> ok;
     true -> erlang:cancel_timer(TimeRef)
  end,
  lager:info("ping timer expired"),
  TimeRefU = send_ping(),
  {reply, {text, <<"ping">>}, State#{time_ref => TimeRefU}};
websocket_info(Msg, State) ->
  lager:debug("Msg : ~p", [Msg]),
  {ok, State}.

terminate(Reason, _PReq, _State) ->
  lager:info("Terminated with reason :~p", [Reason]),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_message(Message, State) -> {Message, State}.

send_ping() ->
  erlang:send_after(10000, self(), <<"ping">>).

