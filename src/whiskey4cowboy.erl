-module(whiskey4cowboy).

-export([parse_transform/2]).

-define(d(Msg, Args), io:format(?MODULE_STRING ++ ": " ++ Msg ++ "~n", Args)).

-record(attribute, {line, name, value}).
-record(function, {line, name, arity, clauses}).
-record(clause, {line, patterns, guards, body}).
-record('case', {line, body, clauses}).
-record('try', {line, body, clauses, catch_clauses, 'after'}).
-record(var, {line, name}).

parse_transform(Forms, _Options) ->
  forms([], Forms).

forms(Parsed, [F = #attribute{name = module, value = Module} | Rest])
  when
    Module =:= cowboy_handler;
    Module =:= cowboy_http;
    Module =:= cowboy_protocol;
    Module =:= cowboy_req;
    Module =:= cowboy_rest;
    Module =:= cowboy_router;
    Module =:= cowboy_websocket;
    Module =:= cow_cookie;
    Module =:= cow_mimetypes;
    Module =:= cow_qs;
    Module =:= cow_spdy;
    Module =:= ranch_tcp;
    Module =:= ranch_ssl;
    Module =:= ranch_transport;
    false
    ->
  remove_exception_handlers([F | Parsed], Module, Rest);
forms(Parsed, [F = #attribute{name = module, value = Module} | Rest])
  when
    Module =:= ranch_conns_sup;
    false
    ->
  add_sup_error_msg([F | Parsed], Module, Rest);
forms(Parsed, [F | Rest]) ->
  forms([F | Parsed], Rest);
forms(Parsed, []) ->
  lists:reverse(Parsed).

remove_exception_handlers(Parsed, Module, [#function{clauses = Clauses} = Fun | Rest]) ->
  NewClauses = remove_exception_handlers([], Module, Clauses),
  remove_exception_handlers([Fun#function{clauses = NewClauses} | Parsed], Module, Rest);

remove_exception_handlers(Parsed, Module, [#'case'{clauses = C} = Case | Rest]) ->
  NewClauses = remove_exception_handlers([], Module, C),
  remove_exception_handlers([Case#'case'{clauses = NewClauses} | Parsed], Module, Rest);

remove_exception_handlers(Parsed, Module, [#clause{body = Body} = Clause | Rest]) ->
  NewBody = remove_exception_handlers([], Module, Body),
  remove_exception_handlers([Clause#clause{body = NewBody} | Parsed], Module, Rest);

remove_exception_handlers(Parsed, Module, [#'try'{
                                              line = L,
                                              body = [B],
                                              clauses = C,
                                              'after' = A} | Rest])
  when C =/= [], A =:= [] ->
  NewForm = #'case'{line = L, body = B, clauses = C},
  ?d("replaced try with case in ~p:~p", [Module, L]),
  remove_exception_handlers([NewForm | Parsed],  Module, Rest);

remove_exception_handlers(Parsed, Module, [#'try'{
                                              line = L,
                                              body = B,
                                              clauses = [],
                                              'after' = []
                                             } | Rest]) ->
  ?d("removed try from ~p:~p", [Module, L]),
  remove_exception_handlers(lists:reverse(B) ++ Parsed, Module, Rest);

remove_exception_handlers(Parsed, Module, [F = #'try'{} | Rest]) ->
  ?d("not parsed try at ~p:~p", [Module, F#'try'.line]),
  remove_exception_handlers([F | Parsed], Module, Rest);

remove_exception_handlers(Parsed, Module, [F | Rest]) ->
  remove_exception_handlers([F | Parsed], Module, Rest);
remove_exception_handlers(Parsed, _Module, []) ->
  lists:reverse(Parsed).

add_sup_error_msg(Parsed, Module, [#function{
                                      name = report_error,
                                      arity = 4,
                                      clauses = Clauses} = Fun | Rest])
  when Module =:= ranch_conns_sup ->
  [
   #clause{
      line = Line,
      patterns = [
                  #var{name = 'Ref'},
                  #var{name = 'Protocol'},
                  #var{name = 'Pid'},
                  #var{name = 'Reason'}
                 ]
     } = ReportClause | IgnoreClauses] = lists:reverse(Clauses),
  NewReportClause = ReportClause#clause{
                      body = [{match, Line+1,
                               {var,Line+1,'ErrorMsg'},
                               {cons,Line+1,
                                {tuple, Line+1,
                                 [{atom,Line+1,supervisor},
                                  {tuple,Line+1,
                                   [{atom,Line+1,Module},
                                    {var,Line+1,'Ref'},
                                    {var,Line+1,'Protocol'}]}]},
                                {cons,Line+1,
                                 {tuple,Line+1,
                                  [{atom,Line+1,errorContext},
                                   {atom,Line+1,child_terminated}]},
                                 {cons,Line+1,
                                  {tuple,Line+1,[{atom,Line+1,reason},{var,Line+1,'Reason'}]},
                                  {cons,Line+1,
                                   {tuple,Line+1,[{atom,Line+1,offender},{var,Line+1,'Pid'}]},
                                   {nil,Line+1}}}}}},
                              {call,Line+2,
                               {remote,Line+2,{atom,Line+2,error_logger},{atom,Line+2,error_report}},
                               [{atom,Line+2,supervisor_report},{var,Line+2,'ErrorMsg'}]}
                             ]
                     },
  NewClauses = lists:reverse([NewReportClause | IgnoreClauses]),
  forms([Fun#function{clauses = NewClauses} | Parsed], Rest);
add_sup_error_msg(Parsed, Module, [F | Rest]) ->
  add_sup_error_msg([F | Parsed], Module, Rest).
