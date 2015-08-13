-module(w4c_ng).

-export([template2js/3]).

-record(template2js, {module,
                      path = "",
                      cache = <<"$templateCache">>,
                      depth_start = 1
                     }).

template2js(Path, Html, #template2js{depth_start = DepthStart}) when length(Path) < DepthStart ->
  {ok, Html};
template2js(Path, Html, #template2js{module = Module,
                                     path = PathPrefix,
                                     cache = Cache,
                                     depth_start = DepthStart}) ->
  CuttedPath = lists:nthtail(DepthStart, lists:reverse(Path)),
  JS = iolist_to_binary(["angular.module('", Module, "').run(['", Cache, "', function(cache) {",
                         "cache.put('", '_':join([PathPrefix | CuttedPath], "/"), "', \""]),
  Html1 = iolist_to_binary(re:replace(Html, "\"", "\\\\\"", [global])),
  Html2 = case iolist_to_binary(re:replace(Html1, "(.*)\n", " \\1", [global])) of
            <<32, Rest/binary>> -> Rest;
            Rest -> Rest
          end,
  {ok, <<JS/binary, Html2/binary, "\")}]);">>};
template2js(Path, Html, Opts) ->
  io:format("paring opts ~p~n",[ Opts]),
  template2js(Path, Html, parse_opts(#template2js{}, Opts)).

parse_opts(Parsed, #{module := Module} = Opts) ->
  parse_opts(Parsed#template2js{module = Module}, maps:remove(module, Opts));
parse_opts(Parsed, #{path := Path} = Opts) ->
  parse_opts(Parsed#template2js{path = Path}, maps:remove(path, Opts));
parse_opts(Parsed, #{cache := Cache} = Opts) ->
  parse_opts(Parsed#template2js{cache = Cache}, maps:remove(cache, Opts));
parse_opts(Parsed, #{depth_start := Depth} = Opts) ->
  parse_opts(Parsed#template2js{depth_start = Depth}, maps:remove(depth_start, Opts));
parse_opts(Parsed, _) ->
  Parsed.
