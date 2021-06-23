%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-04-30 11:45:39.975160
%%%-------------------------------------------------------------------

-module(bs_identity).

-include("bs.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         authenticate/1,
         login/1,
         logout/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

authenticate(Request) ->
%  gen_server:call(?SERVER, {authenticate, Request}).
  handle_authenticate(Request).

login(Request) ->
%  gen_server:call(?SERVER, {login, Request}).
  handle_login(Request).

logout(Request) ->
%  gen_server:call(?SERVER, {logout, Request}).
  handle_logout(Request).

init(_Args) ->
   {ok, #{}}.

handle_call({authenticate, Request}, _From, State) ->
    Reply = handle_authenticate(Request),
    {reply, Reply, State};
handle_call({login, Request}, _From, State) ->
    Reply = handle_login(Request),
    {reply, Reply, State};
handle_call({logout, Request}, _From, State) ->
    Reply = handle_logout(Request),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info({login_exp, SessionId, NowSecs}, State) ->
    check_expired_session(SessionId, NowSecs),
    {noreply, State};
handle_info({update_login_exp, _SessionId, _NowSecs}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_authenticate(SessionId) ->
  lager:info("Authenticate ~p",[SessionId]),
  check_session(SessionId).

handle_login(#{username := UserName, password := Pswd}) ->
  PswdMd5 = erlang:md5(Pswd),
  case mnesia:dirty_index_read(users, UserName, username) of
    [#users{uuid = UserId, role = Role, password = PswdMd5}] ->
      NowSecs = erlang:system_time(seconds),
      check_login(UserId, Role,NowSecs);
    Else  ->
      lager:info("Else ~p",[Else]),
      {error, <<"Invalid user or role!">>}
  end;
handle_login(Req) ->
  lager:info("login request ~p",[Req]),
  {error, <<"Invalid login!">>}.

handle_logout(#{user_id := UserId}) ->
  case mnesia:dirty_index_read(logins, UserId, user_id) of
    [] -> ok;
    OtherLogins ->
      [mnesia:dirty_delete_object(LoginRec) || LoginRec <- OtherLogins],
      ok
  end;
handle_logout(_)  ->
  {error, <<"Mandatory field missing!">>}.

check_session(SessionId) ->
  case mnesia:dirty_read(logins, SessionId) of
    [] -> false;
    [Login] ->
      NowSecs = erlang:system_time(seconds),
      is_session_active(Login, NowSecs)
  end.

is_session_active(#logins{access_ts = LastAccessTs, uuid = SessionId}, NowSecs)
                                                    when NowSecs - LastAccessTs > 3600 ->
  mnesia:dirty_delete(logins, SessionId),
  false;
is_session_active(Login, NowSecs) ->
  mnesia:dirty_write(Login#logins{access_ts = NowSecs}),
  {true, Login#logins.user_id}.

check_login(UserId, Role, NowSecs) ->
  AllLogins =  mnesia:dirty_index_read(logins, UserId, user_id), % of
%    [#logins{access_ts = AccessTS, uuid = SessionId} = Login] when NowSecs - AccessTS < 3600 ->
%      mnesia:dirty_write(Login#logins{access_ts = NowSecs}),
%      {ok, #{user_id => UserId, sessionid => SessionId}};
%    OtherLogins ->
      [mnesia:dirty_delete_object(LoginRec) || LoginRec <- AllLogins],
      SessionId = bs_utils:create_uuid(),
      LoginRec = #logins{
                    uuid = SessionId,
                    user_id = UserId,
                    access_ts = NowSecs},
      mnesia:dirty_write(LoginRec),
      Username = username(UserId),
      {login, #{user => Username,
                user_id => UserId,
                role => Role,
                session_id => SessionId}}.
%  end.

username(UserId) ->
  case mnesia:dirty_read(users, UserId) of
    [] -> <<"UnKnown">>;
    [#users{display_name=Name}] -> Name
  end.

check_expired_session(SessionId, TimeSec) ->
  case mnesia:dirty_read(logins, SessionId) of
    [] -> ok;
    [#logins{access_ts = AccessTS}] when AccessTS - TimeSec > 0 -> ok;
    [OtherLogins] ->
      mnesia:dirty_delete(OtherLogins),
      ok
  end.

