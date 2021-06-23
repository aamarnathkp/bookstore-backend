%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-04-30 11:45:39.975160
%%%-------------------------------------------------------------------

-module(bs_api_rest).

-export([init/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         delete_resource/2,
         terminate/3]).

-export([handle_request/2]).


init(Req, State) ->
    lager:info("Req for REST"),
    {cowboy_rest, Req, State}.

allowed_methods(Req,State) ->
  Methods = [<<"GET">>, <<"POST">>,
             <<"OPTIONS">>, <<"DELETE">>],
  {Methods, Req, State}.

is_authorized(#{method := <<"OPTIONS">>} = Req, State) ->
  ACRequestHeaders = cowboy_req:header(<<"access-control-request-headers">>, Req),
  Req1 = cowboy_req:set_resp_headers(#{<<"content-type">> => <<"text/html; text/plain; application/json;
                                                  chatset=utf-8">>,
                                       <<"Access-Control-Allow-Headers">> => ACRequestHeaders,
                                       <<"Access-Control-Allow-Methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
                                       <<"Access-Control-Max-Age">> => <<"1728000">>
                                      }, Req),
  {true, Req1, State};
is_authorized(Req,State) ->
  Path = cowboy_req:path(Req),
  Body = read_body(Req),
  Method = cowboy_req:method(Req),
  lager:info("path ~p Method ~p Body ~p ",[Path, Method, Body]),
  is_authorized(Path, Req, State#{body => Body}).

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_request},
    {<<"multipart/form-data">>, handle_request}], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, handle_request},
    {<<"multipart/form-data">>, handle_request}], Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

delete_resource(Req, State) ->
  {true, Req, State}.

terminate(Reason, _Req, _State) ->
  lager:info("Requested terminated with ~p",[Reason]),
  ok.


handle_request(Req, State) ->
  Request = frame_request(Req, State),
  lager:info("framed Request ~p",[Request]),
  case make_function_call(Request) of
    ok ->
      ReplyJson = encode_params(#{}),
      Req1 = cowboy_req:set_resp_body(ReplyJson, Req),
      {true, Req1, State};
    {login, #{session_id := SessionId} = ReplyMap} ->
      ReplyJson = encode_params(ReplyMap),
      Req1 = cowboy_req:set_resp_cookie(<<"bs_cookie">>, SessionId, Req),
      ReqU = cowboy_req:set_resp_body(ReplyJson, Req1),
      {true, ReqU, State};
    {ok, ReplyMap} ->
%      lager:info("ReplyMap ~p ",[ReplyMap]),
      ReplyJson = encode_params(ReplyMap),
%      lager:info("RJSON ~p",[ReplyJson]),
      case cowboy_req:method(Req) of
        <<"GET">> ->
         {ReplyJson, Req, State};
        <<"POST">> ->
          ReqU = cowboy_req:set_resp_body(ReplyJson, Req),
          {true, ReqU, State}
      end;
    {error, Reason} ->
      lager:info("api error ~p ",[Reason]),
      ReplyJson = encode_params({failed_reason, bs_utils:to(binary, Reason)}),
      Req1 = cowboy_req:reply(400, #{}, ReplyJson, Req),
      {false, Req1, State};
    Error ->
      lager:info("api error else ~p ",[Error]),
      ReplyJson = encode_params({failed_reason, <<"not_authorized">>}),
      Req1 = cowboy_req:reply(401, #{}, ReplyJson, Req),
      {false, Req1, State}
  end.

frame_request(Req, State) ->
  #{method := Method} = Req,
  #{body := Body} = State,
  #{path_info := PathInfo} = Req,
  Pages = query_string(Method, Req),
  UserId = maps:get(user_id, State, undefined),
  Body#{method => Method, user_id => UserId,
        path => PathInfo, pages => Pages}.

query_string(<<"GET">>, Req) ->
  #{data := Data} = cowboy_req:match_qs([{data, [], <<"{}">>}], Req),
  bs_utils:decode_json(Data, [return_maps]);
query_string(_, _) -> #{}.

make_function_call(Request) ->
  bs_api_routes:api_routes(Request).

read_body(Req) ->
  read_body(Req, <<>>).

read_body(Req, Acc) ->
  HasBody = cowboy_req:has_body(Req),
  %{ok, Arguments, Req2} = cowboy_req:read_body(Req, #{length => infinity}),
  ContentType = cowboy_req:header(<<"content-type">>, Req),
  case catch try_collate_multipart(ContentType, Req) of
    {'EXIT', CrashMP} ->
      lager:error("Crashed while trying multipart parsing ~p", [CrashMP]),
      api_read_body(HasBody, Req, Acc);
    {ok, ArgMaps} ->
      lager:debug("Multipart arguments are ~p", [ArgMaps]),
      ArgMaps;
    Other ->
      lager:debug("Not a multipart so doing normal parsing ~p", [Other]),
       api_read_body(HasBody, Req, Acc)
  end.

%  api_read_body(HasBody, Req, Acc).

api_read_body(false, _Req, <<>>) -> #{};
api_read_body(false, _Req, Acc) -> Acc;
api_read_body(true, Req, Acc) ->
  case cowboy_req:read_body(Req) of
    {ok, Data, _Req1} ->
      lager:info("Acc: ~p, Data ~p:",[Acc, Data]),
      Body = << Acc/binary, Data/binary>>,
      bs_utils:decode_params(Body, [return_maps]);
    {more, Data, Req1} ->
      read_body(Req1, << Acc/binary, Data/binary >>)
  end.


encode_params({failed_reason, Reason}) ->
  lager:debug("Encoding failed_reason ~p to json", [Reason]),
  bs_utils:encode_params(#{success => false, reason => Reason});
encode_params(ResponseMap) ->
  ResponseMapU = bs_utils:filter_map_recursive(ResponseMap, undefined),
%  ResponseMapU2 = bs_utils:filter_map_recursive(ResponseMapU, []),
  bs_utils:encode_params(ResponseMapU#{success => true}).

is_authorized(<<"/login">>, Req, State) ->
  {true, Req, State};
is_authorized(<<"/guest/authors">>, Req, State) ->
  {true, Req, State};
is_authorized(<<"/guest/books">>, Req, State) ->
  {true, Req, State};
is_authorized(<<"/guest/counts">>, Req, State) ->
  {true, Req, State};
is_authorized(_, Req, State) ->
  Cookies = cowboy_req:parse_cookies(Req),
  BSSession = proplists:get_value(<<"bs_cookie">>, Cookies),
  lager:info("Bs cookie ~p ",[BSSession]),
  case bs_identity:authenticate(BSSession) of
    {true, UserId} ->
      {true, Req, State#{user_id => UserId}};
    _ ->
      {{false, <<>>}, Req, State}
  end.

try_collate_multipart(<<"multipart/form-data; boundary=", Boundary/binary>>,
                      Req) ->
  {ok, Arguments, _Req} = cowboy_req:read_body(Req, #{length => infinity}),
  lager:debug("Trying to decode multipart with ~p boundary and ~p arguments",
              [Boundary, Arguments]),
  case hackney_multipart:decode_form(Boundary, Arguments) of
    {ok, Parts} ->
      lager:debug("Successfully able to decode multipart into ~p",
                  [Parts]),
      multipart_to_map(Parts);
    Error ->
      lager:error("OOPS! Failed to decode multipart ~p", [Error]),
      Error
  end;
try_collate_multipart(Other, _Arguments) ->
  lager:debug("Not a multipart message ~p", [Other]),
  {error, "not_a_multipart"}.

multipart_to_map(Parts) ->
  multipart_to_map(Parts, #{}).

multipart_to_map([], ArgsMap) ->
  {ok, ArgsMap};
multipart_to_map([{[{<<"Content-Disposition">>, FormName}], FormData}|Parts],
                 ArgsMap) ->
  <<"form-data; name=", NameRaw/binary>> = FormName,
  Name = bs_utils:to(atom, binary:replace(NameRaw, <<"\"">>, <<>>, [global])),
  Data = bs_utils:to(binary, FormData),
  multipart_to_map(Parts, ArgsMap#{Name => Data});
multipart_to_map([{[{<<"Content-Disposition">>, _FormFields},
                    {<<"Content-Type">>, ContentType}], FileData}|Parts],
                 ArgsMap) ->
  multipart_to_map(Parts, ArgsMap#{content_type => ContentType,
                                   file_data => FileData}).


