%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-04-29 14:56:46.072657
%%%-------------------------------------------------------------------



%%%===================================================================
%%% Book Store  Table Definitions
%%%===================================================================

-record(logins, {
          uuid,
          user_id,
          access_ts
        }).

-record(users, {
          uuid,
          username, %% unique entire system
          password,
          display_name,
          role,     %% admin / user
          status,   %% active / inactive
          user_books_rl,
          created_on,
          created_by,
          modified_on,
          modified_by
         }).

-record(user_books_r, {
          currect_books, % {book id, borrow id}
          previous_books = [] % list of book ids
         }).

-record(authors, {
          uuid,
          name,
          about,
          image,
          status,
          created_on,
          created_by,
          modified_on,
          modified_by
         }).


-record(books, {
          uuid,
          name,
          author_uuid,
          image,
          description,
          published_year,
          page_count,
          genres, %%Fiction, Novel, Thriller, Narrative etc
          language,
          price,
          in_stock,
          count,
          book_shelf_uuid,
          created_on,
          created_by,
          modified_on,
          modified_by
         }).

-record(book_reviews, {
          uuid,
          book_uuid,
          author_uuid,
          reviewed_on,
          user_uuid
         }).

-record(book_shelf, {
          uuid,
          name,
          notes,
          shelf_type, %% book genres
          language,
          max_shelf_rows,
          max_books_in_row,
          book_shelf_rows_map,%% #{ 1 => [book_uuids], 2 => [book_uuids], ..},
          created_on,
          created_by,
          modified_on,
          modified_by
         }).

-record(book_shelf_rows_r, {
          row_number,
          compartment,
          books %[book_uuids]
         }).


-record(book_borrow, {
          uuid,
          user_uuid,
          book_uuid,
          due_date,
          borrowed_on,
          approved_by % admin_uuid
         }).

-record(book_store_rules, {
          uuid,
          user_borrow_limit,
          fine_amount,
          rent_amount,
          info_map
         }).


