%%%-------------------------------------------------------------------
%%% @author aamarnathkp
%%% @copyright (C) 2021, aamarnathkp
%%% @doc
%%%
%%% @end
%%% Created : 2021-05-18 17:54:39.565315
%%%-------------------------------------------------------------------



%%%===================================================================
%%% Selection Table Definitions
%%%===================================================================


-record(exam, {
          uuid,
          name,
          type,      % HR/Dev/QA/others
          duration,  % exam duration
          questions_rl = [],
          created_by,
          created_on,
          modified_by,
          modified_on
         }).

-record(questions_r, {
          question_uuid,
          order_no
         }).

-record(questions, {
          uuid,
          name,
          type, % aptitude/general/programming/logical
          answer_type, % boolean, multiple_choice, coding
          answers_rl,
          score,       % total score for correct answer
          created_by,
          created_on,
          modified_by,
          modified_on
         }).

-record(answers_rl, {
          answer_text,
          score   % 0 means false, > 0 means true
         }).

-record(answer_sheets, {
          uuid,
          user_uuid,
          exam_uuid,
          duration,  %{start_time, end_time}
          answer_sheet = [] %[{question_uuid, answer_text, score},...]
          total_score,
          status, % timer expired/ completed/ early completion
          created_on
         }).













