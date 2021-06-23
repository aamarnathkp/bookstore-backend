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
         list_all/1
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

list_all(Req) ->
  handle_list_all(Req).

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


handle_modify(#{uuid := Author}=Req) ->
  lager:info("author modify request ~p ",[Req]),
  case mnesia:dirty_read(authors, Author) of
    [] ->
      {error, "Author not Found"};
    [Rec] ->
%      UuidStr = bs_utils:to(string, Author),
%      ImageName = format_image_name(Req, UuidStr),
%      UploadPath = bs_utils:bs_image_path(ImageName),
%      ok = file:write_file(UploadPath, FileData),
%      lager:info("Uuid ~p", [UuidStr]),
%      %http://localhost:7778/images/sound.jpeg
%      ImagePath = "http://localhost:7778/images/",
%      ImageRelativePath = bs_utils:to(binary, ImagePath ++ ImageName),
%      mnesia:dirty_write(Rec#authors{image = ImageRelativePath}),

      Name = maps:get(name, Req, Rec#authors.name),
      About = maps:get(about, Req, Rec#authors.about),
      Image = author_image(Req, Rec),
      mnesia:dirty_write(Rec#authors{
                           name = Name,
                           about = About,
                           image = Image}),
      ok
  end;
handle_modify(Request) ->
  lager:info("Req ~p",[Request]),
  {error, "madatory fields are missing"}.

author_image(#{file_data := FileData} = Req, Rec) ->
  UuidStr = bs_utils:to(string, Rec#authors.uuid),
  ImageName = format_image_name(Req, UuidStr),
  UploadPath = bs_utils:bs_image_path(ImageName),
  ok = file:write_file(UploadPath, FileData),
  ImagePath = "http://localhost:7778/images/",
  bs_utils:to(binary, ImagePath ++ ImageName);
author_image(_, Rec) -> Rec#authors.image.

handle_list_by_users(#{user_id := User }) ->
  Authors = mnesia:dirty_index_read(authors, User, created_by),
  list_authors(Authors);
handle_list_by_users(_Request) ->
  {error, "User not found"}.

handle_list_all(Req) ->
  lager:info("Req ~p", [Req]),
  Pages = maps:get(pages, Req, []),
  Authors = [ mnesia:dirty_read(authors, Id) || Id <- mnesia:dirty_all_keys(authors)],
  AuthorsU = pagination(Pages, Authors),
  list_authors(AuthorsU).

list_authors(Authors) ->
  Fields = record_info(fields, authors),
  AuthorsMap = bs_utils:rec_to_map(Fields, Authors),
  Total = length(AuthorsMap),
  {ok, #{total => Total, %bs_utils:to(binary, Total),
         items => AuthorsMap}}.

format_image_name(#{content_type := <<"image/png">>}, UuidStr) -> UuidStr ++ ".png";
format_image_name(_, UuidStr) -> UuidStr ++ ".jpeg".

pagination(#{<<"min">> := Min, <<"max">> := Max}, Authors) ->
  lager:info("Authors ~p ",[Authors]),
  lists:sublist(Authors, Min+1, Max);
pagination(_, Authors) -> Authors.



