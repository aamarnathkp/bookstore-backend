%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-05-03 15:01:01.055824
%%%-------------------------------------------------------------------

-module(bs_authors).

-include("bs.hrl").

%% API
-export([add_api/1,
         modify_api/1,
         list_by_users/1,
         list_all/0
        ]).

-export([count/0]).

%%%===================================================================
%%% API
%%%===================================================================

add_api(#{name := _Name} = Req) ->
  handle_add(Req);
add_api(_) ->
  {error,  "Author Name is missing"}.

modify_api(Request) ->
  handle_modify(Request).

list_by_users(Request) ->
  handle_list_by_users(Request).

list_all() ->
  handle_list_all().

%%%===================================================================
%%% Utilities
%%%===================================================================

count() -> length(mnesia:dirty_all_keys(authors)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_add(#{name := Name}=Req) ->
  About = maps:get(about, Req, undefined),
  UserId = maps:get(user_id, Req, superadmin),
  Uuid = bs_utils:create_uuid(),
  mnesia:dirty_write(#authors{uuid = Uuid,
                              name = Name,
                              about= About,
                              status = active,
                              created_by = UserId}),
  {ok, #{uuid => Uuid}}.


handle_modify(#{uuid := Author, file_data := FileData}=Req) ->
  lager:info("upload data == ~p ",[FileData]),
  case mnesia:dirty_read(authors, Author) of
    [] ->
      {error, "Author not Found"};
    [Rec] ->
      UuidStr = bs_utils:to(string, Author),
      ImageName = format_image_name(Req, UuidStr),
      UploadPath = bs_utils:bs_image_path(ImageName),
      ok = file:write_file(UploadPath, FileData),
      lager:info("Uuid ~p", [UuidStr]),
      %http://localhost:7778/images/sound.jpeg
      ImagePath = "http://localhost:7778/images/",
      ImageRelativePath = bs_utils:to(binary, ImagePath ++ ImageName),
      mnesia:dirty_write(Rec#authors{image = ImageRelativePath}),
      ok
  end;
handle_modify(Request) ->
  lager:info("Req ~p",[Request]),
  {error, "madatory fields are missing"}.

handle_list_by_users(#{user_id := User }) ->
  Authors = mnesia:dirty_index_read(authors, User, created_by),
  list_authors(Authors);
handle_list_by_users(_Request) ->
  {error, "User not found"}.

handle_list_all() ->
  Authors = [ mnesia:dirty_read(authors, Id) || Id <- mnesia:dirty_all_keys(authors)],
  list_authors(Authors).

list_authors(Authors) ->
  Fields = record_info(fields, authors),
  AuthorsMap = bs_utils:rec_to_map(Fields, Authors),
  Total = length(AuthorsMap),
  {ok, #{total => bs_utils:to(binary, Total),
         items => AuthorsMap}}.

format_image_name(#{content_type := <<"image/png">>}, UuidStr) -> UuidStr ++ ".png";
format_image_name(_, UuidStr) -> UuidStr ++ ".jpeg".



