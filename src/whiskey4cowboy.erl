-module(whiskey4cowboy).

-export([parse_transform/2]).

-define(d(Msg, Args), io:format(?MODULE_STRING ++ ": " ++ Msg ++ "~n", Args)).

parse_transform(Forms, Options) ->
  ?d("parsing ~p", [Forms]),
  ?d("options ~p", [Options]),
  Forms.
