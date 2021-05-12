%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-05-06 07:17:06.195284
%%%-------------------------------------------------------------------

-module(bs_books).

-include("bs.hrl").

%% API
-export([add_api/1,
         modify_api/1,
         list_all/0]).

-export([count/0]).

%%%===================================================================
%%% API
%%%===================================================================

add_api(Request) ->
  handle_add_api(Request).

modify_api(Request) ->
  handle_modify(Request).

list_all() ->
  handle_list_all().

%%%===================================================================
%%% Utilities
%%%===================================================================

count() -> length(mnesia:dirty_all_keys(books)).


%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_add_api(#{name:= BookName, author_uuid := Author}=Request) ->
  %TODO validate author id
  Image = maps:get(image, Request, undefined),
  Description = maps:get(description, Request, undefined),
  PublishYear = maps:get(published_year, Request, undefined),
  Price = maps:get(price, Request, 0),
  Uuid = bs_utils:create_uuid(),
  mnesia:dirty_write(#books{uuid = Uuid,
                            name = BookName,
                            author_uuid = Author,
                            image = Image,
                            description = Description,
                            published_year = PublishYear,
                            price = Price
                       }),
  {ok, #{uuid => Uuid}}.

handle_modify(#{uuid := Book, file_data := FileData}=Req) ->
  lager:info("upload data == ~p ",[FileData]),
  case mnesia:dirty_read(books, Book) of
    [] ->
      {error, "Book not Found"};
    [Rec] ->
      UuidStr = bs_utils:to(string, Book),
      ImageName = format_image_name(Req, UuidStr),
      UploadPath = bs_utils:bs_image_path(ImageName),
      ok = file:write_file(UploadPath, FileData),
      lager:info("Uuid ~p", [UuidStr]),
      %http://localhost:7778/images/sound.jpeg
      ImagePath = "http://localhost:7778/images/",
      ImageRelativePath = bs_utils:to(binary, ImagePath ++ ImageName),
      mnesia:dirty_write(Rec#books{image = ImageRelativePath}),
      ok
  end;
handle_modify(Request) ->
  lager:info("Req ~p",[Request]),
  {error, "madatory fields are missing"}.


handle_list_all() ->
  Books = [ mnesia:dirty_read(books, Id) || Id <- mnesia:dirty_all_keys(books)],
  list_books(Books).

list_books(Books) ->
  Fields = record_info(fields, books),
  BooksMap = bs_utils:rec_to_map(Fields, Books),
  Total = length(BooksMap),
  {ok, #{total => bs_utils:to(binary, Total),
         items => BooksMap}}.

format_image_name(#{content_type := <<"image/png">>}, UuidStr) -> UuidStr ++ ".png";
format_image_name(_, UuidStr) -> UuidStr ++ ".jpeg".

