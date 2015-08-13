-module(w4c).

-export([path/1]).
-export([path_tokens/1]).

path(Req) ->
  {Path, Req} = cowboy_req:path(Req),
  Path.

path_tokens(Req) ->
  [X || X <- re:split(path(Req), "/"), X =/= <<>>].

%method(Req) ->
