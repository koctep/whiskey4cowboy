-module(w4c_mimetypes).

-export([web/1]).

web(Path) ->
  {Type, SubType, _Params} = cow_mimetypes:web(Path),
%  Params = [{"charset", "utf-8"}],
  Params = [],
  {Type, SubType, Params}.
