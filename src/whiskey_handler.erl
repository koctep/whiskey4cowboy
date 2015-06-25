-module(whiskey_handler).

-export([execute/2]).
-export([run_cowboy_handler/3]).

execute(Req, State) ->
  Socket = element(2, Req),
  {CHPid, Mref} = spawn_monitor(?MODULE, run_cowboy_handler, [self(), Req, State]),
  gen_tcp:controlling_process(Socket, CHPid),
  CHPid ! execute,
  receive
    {CHPid, Reply} ->
      erlang:demonitor(Mref),
      maybe_update_charset(Reply);
    {'DOWN', Mref, process, CHPid, _Reason} ->
      {error, 500, Req}
  end.

run_cowboy_handler(From, Req, State) ->
  Socket = element(2, Req),
  Reply = receive
            execute ->
              Reply1 = cowboy_handler:execute(Req, State),
              gen_tcp:controlling_process(Socket, From),
              Reply1
          end,
  From ! {self(), Reply}.

maybe_update_charset({ok, Req, Env}) ->
  {ok, update_charset(Req), Env};
maybe_update_charset({halt, Req}) ->
  {halt, update_charset(Req)};
maybe_update_charset(Reply) ->
  Reply.

update_charset(Req) ->
  case cowboy_req:meta(charset, Req) of
    {undefined, Req} ->
      cowboy_req:set_meta(charset, <<"utf-8">>, Req);
    {_, Req} ->
      Req
  end.
