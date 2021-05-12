%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-04-30 11:45:39.975160
%%%-------------------------------------------------------------------

-module(bs_utils).

-include("bs.hrl").

-export([get_bs_home/0,
         bs_image_path/1,
         web_data_path/0,
         encode_params/1,
         encode_json/1,
         decode_params/1,
         decode_params/2,
         decode_json/2,
         create_uuid/0,
         bs_now/0,
         is_username_unique/1,
         is_name_unique/2,
         create_superadmin/0,
         ts_to_bin/1,
         ts_to_str/1,
         rec_to_map/2,
         to/2,
         filter_map_recursive/2
         ]).

get_bs_home() ->
  {ok, CWD} = file:get_cwd(),
  os:getenv("BS_HOME", CWD).

bs_image_path(ImageId) ->
  BSHOME = get_bs_home(),
  BSHOME ++ "/var/data/images/" ++ ImageId.

web_data_path() ->
  BSHOME = get_bs_home(),
  DataPath = "var/data",
  Path = format("~s/~s", [BSHOME, DataPath]),
  lager:info("Web data path is ~p", [Path]),
  Path.

format(Format, ParamList) ->
  lists:flatten(io_lib:format(Format, ParamList)).

encode_params(ErlangParams) ->
  encode_json(ErlangParams).

encode_json(ErlangTerm) when is_tuple(ErlangTerm) ->
  jiffy:encode({[ErlangTerm]}, [force_utf8]);
encode_json(ErlangTerm) ->
  jiffy:encode(ErlangTerm, [force_utf8]).

%% Decode json to erlang param list
%% decode_params(JsonParams) -> ErlangTermList
decode_params(JsonParams) ->
  decode_params(JsonParams, []).

decode_params(JsonParams, Options) ->
  case decode_json(JsonParams, Options) of
    ErlangMap when is_map(ErlangMap) ->
      atomize_keys(ErlangMap);
    ErlangParams ->
      [{to(atom, Param), Value} || {Param, Value} <- ErlangParams]
  end.

atomize_keys(ErlangMapList) when is_list(ErlangMapList) ->
  [atomize_keys(ErlangMap) || ErlangMap <- ErlangMapList];
atomize_keys(ErlangMap) when is_map(ErlangMap) ->
  ErlangList = maps:to_list(ErlangMap),
  maps:from_list(atomize_keys(ErlangList, []));
atomize_keys(ErlangTerm) ->
  ErlangTerm.

atomize_keys([], ErlangListA) -> lists:reverse(ErlangListA);
atomize_keys([{Key, Value}|ErlangList], ErlangListA) when is_map(Value) ->
  atomize_keys(ErlangList,
               [{to(atom, Key), atomize_keys(Value)}|ErlangListA]);
atomize_keys([{Key, Value}|ErlangList], ErlangListA) when is_list(Value) ->
  atomize_keys(ErlangList,
               [{to(atom, Key), atomize_keys(Value)}|ErlangListA]);
atomize_keys([{Key, Value}|ErlangList], ErlangListA) ->
  atomize_keys(ErlangList,
               [{to(atom, Key), Value}|ErlangListA]).

%% Decode json to erlang term
%% @spec decode_json(JsonParams) -> ErlangTerm
decode_json(JsonBinary, Options) ->
  case jiffy:decode(JsonBinary, Options) of
    {ErlangTerm} -> ErlangTerm;
    Other -> Other
  end.

create_uuid() ->
  list_to_binary(uuid_str()).

uuid_str() ->
  uuid:uuid_to_string(uuid:get_v4()).

bs_now() ->
  os:timestamp().

is_username_unique(UserName) ->
  case mnesia:dirty_index_read(users, UserName, username) of
    [] -> true;
    _  -> {false, <<UserName/binary, " already in use!">>}
  end.

is_name_unique(TableName, Name) ->
  case mnesia:dirty_index_read(TableName, Name, name) of
    [] -> true;
    _  -> {false, <<Name/binary, " already in use">>}
  end.

create_superadmin() ->
    case mnesia:dirty_index_read(users, <<"superadmin">>, username) of
        [] ->
            LoginRec = #users{
                            uuid  = <<"superadmin">>,
                            username = <<"admin@gmail.com">>,
                            password = erlang:md5(<<"superadmin">>),
                            display_name = <<"SuperAdmin">>,
                            role = admin,
                            created_on = bs_now(),
                            created_by = <<"superadmin">>
                        },
            mnesia:dirty_write(LoginRec);
        _SuperAdminRec ->
            {error, <<"SuperAdmin already created!!">>}
    end.

ts_to_bin(ErlangNow) ->
  list_to_binary(ts_to_str(ErlangNow)).

ts_to_str(Nanoseconds) when is_integer(Nanoseconds) ->
  ErlangTS = Nanoseconds div 1000,
  Megaseconds = ErlangTS div 1000000000000,
  Secs = ErlangTS div 1000000 - Megaseconds * 1000000,
  Microseconds = ErlangTS rem 1000000,
  ts_to_str({Megaseconds, Secs, Microseconds});
ts_to_str({MegaSecs, Secs, MicroSecs}) ->
  {{YYYY, MM, DD}, {H, M, S}} =
  calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
  MS = trunc(MicroSecs / 1000),
  lists:flatten(
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0w",
                  [YYYY, MM, DD, H, M, S, MS]));
ts_to_str({{YYYY, MM, DD}, {H, M, S}}) ->
  lists:flatten(
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                  [YYYY, MM, DD, H, M, S])).

rec_to_map(Fields, RecsList) when is_list(RecsList) ->
  lists:flatten([rec_to_map(Fields, Rec) || Rec <- RecsList]);
rec_to_map(Fields, Rec) ->
  [_| Values] = tuple_to_list(Rec),
  ValuesU = lists:map( fun(undefined) -> []; (Any) -> Any end, Values ),
  maps:from_list(lists:zip(Fields, ValuesU)).

to(int, Data) ->
  to_int(Data);
to(float, Data) ->
  to_float(Data);
to(list, Data) ->
  to_list(Data);
to(atom, Data) ->
  to_atom(Data);
to(string, Data) ->
  to_string(Data);
to(string_lower, Data) ->
  string:to_lower(to_string(Data));
to(binary, Data) ->
  to_binary(Data);
to(timestamp, Data) ->
  to_timestamp(Data).

to_int(Data) when is_atom(Data) ->
  to_int(atom_to_list(Data));
to_int(Data) when is_binary(Data) ->
  to_int(binary_to_list(Data));
to_int(Data) when is_list(Data) ->
  list_to_integer(Data);
to_int(Data) when is_float(Data) ->
  round(Data);
to_int(Data) when is_integer(Data) ->
  Data.

to_float(Data) when is_binary(Data) ->
  binary_to_float(Data);
to_float(Data) when is_atom(Data) ->
  to_float(atom_to_list(Data));
to_float(Data) when is_list(Data) ->
  list_to_float(Data);
to_float(Data) when is_integer(Data) ->
  Data/1;
to_float(Data) when is_float(Data) ->
  Data.

to_list(Data) when is_binary(Data) ->
  binary_to_list(Data);
to_list(Data) when is_atom(Data) ->
  atom_to_list(Data);
to_list(Data) when is_float(Data) ->
  float_to_list(Data, [{decimals, 2}, compact]);
to_list(Data) when is_integer(Data) ->
  integer_to_list(Data);
to_list(Data) when is_list(Data) ->
  Data.

to_atom(Data) when is_binary(Data) ->
  to_atom(binary_to_list(Data));
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_integer(Data) ->
  to_atom(integer_to_list(Data));
to_atom(Data) when is_float(Data) ->
  to_atom(float_to_list(Data, [{decimals, 2}, compact]));
to_atom(Data) when is_atom(Data) ->
  Data.

to_string(Data) when is_list(Data) ->
  case io_lib:printable_list(Data) of
    true ->
      Data;
    false ->
      lists:flatten(io_lib:format("~p", [Data]))
  end;
to_string(Data) ->
  to_string(to_list(Data)).

to_binary(Data) when is_list(Data) ->
  list_to_binary(handle_unicode(Data));
to_binary(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_binary(Data) when is_float(Data) ->
  float_to_binary(Data, [{decimals, 2}, compact]);
to_binary(undefined) ->
  <<>>;
to_binary(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_binary(Data) when is_binary(Data) ->
  Data;
to_binary(_Data) ->
  <<>>.

to_timestamp({{YYYY, MM, DD}, {H, M, S}}) ->
  DateTime = {{YYYY, MM, DD}, {H, M, S}},
  Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
  {Seconds div 1000000, Seconds rem 1000000, 0};
to_timestamp({MegaSecs, Secs, MicroSecs}) ->
  {MegaSecs, Secs, MicroSecs}.

handle_unicode(Data) ->
  lists:foldr(
    fun(C, A) when C < 256 ->
        [C|A];
       (C, A) ->
        binary_to_list(<<C/utf8>>) ++ A
    end,
    [],
    Data
   ).

filter_map_recursive(Map, Match) ->
  filter_map_recursive(Map, Match, maps:keys(Map), #{}).

filter_map_recursive(_Map, _Match, [], MapU) ->
  MapU;
filter_map_recursive(Map, Match, [Key|Keys], MapU) ->
  case maps:get(Key, Map) of
    Match ->
      filter_map_recursive(Map, Match, Keys, MapU);
    Value when is_map(Value) ->
      ValueU = filter_map_recursive(Value, Match),
      filter_map_recursive(Map, Match, Keys, MapU#{Key => ValueU});
    Value ->
      filter_map_recursive(Map, Match, Keys, MapU#{Key => Value})
  end.

