%%%-------------------------------------------------------------------
%%% @author Ilya Ashchepkov
%%% @copyright 2015 Ilya Ashchepkov
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(whiskey_cola).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([last_modified/2]).
-export([compile/2]).

-record(state, {
          result = <<>>,
          mtime = {{1970, 1, 1}, {0, 0, 0}},
          path = [],
          files = [],
          suffix = <<>>,
          suflen = 0,
          recursive = true,
          depth = infinity,
          preadd
         }
       ).

-include_lib("kernel/include/file.hrl").

init(_Type, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
  State = parse_opts(Opts, #state{}),
  NewState = case State#state.files of
               [] -> add_dir(State);
               Files -> add_files(Files, State)
             end,
  {ok, Req, NewState}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"text/css">>, compile}, {<<"application/javascript">>, compile}], Req, State}.

charsets_provided(Req, State) ->
  {[<<"utf-8">>], Req, State}.

%languages_provided(Req, State) ->
%  {[<<"ru">>, <<"en">>], Req, State}.

last_modified(Req, #state{mtime = MTime} = State) ->
  {MTime, Req, State}.

compile(Req, #state{result = Res} = State) ->
  {Res, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_dir(#state{path = Path} = State) ->
  Dir = '_':join(lists:reverse(Path), "/"),
  {ok, Files} = file:list_dir(Dir),
  add_files(Files, State).

add_files([File | Files], #state{path = Path,
                                 recursive = Recursive,
                                 depth = Depth,
                                 mtime = MTime,
                                 suffix = Suffix,
                                 suflen = SufLen
                                } = State) ->
  FullName = '_':join(lists:reverse([File | Path]), "/"),
  {ok, #file_info{type = Type, mtime = Time}} =
    case file:read_file_info(FullName, [{time, universal}]) of
      {error, enoent} ->
        throw({"file not found", FullName});
      Info -> Info
    end,
  NewState = case Type of
               directory when Recursive andalso Depth > 0 ->
                 NewDepth = case Depth of
                              infinity -> infinity;
                              D when is_integer(D) -> D - 1
                            end,
                 add_dir(State#state{path = [File | Path], depth = NewDepth});
               regular ->
                 case list_to_binary('_':reverse(File)) of
                   <<Suffix:SufLen/binary, _/binary>> ->
                     NS = add_file(State#state{path = [File | Path]}),
                     case Time > MTime of
                       true -> NS#state{mtime = Time};
                       false -> NS
                     end;
                   _ ->
                     State
                 end;
               _ ->
                 State
             end,
  add_files(Files, NewState#state{path = Path});
add_files([], State) ->
  State.

add_file(#state{preadd = PreAdd, path = Path, result = Res} = State) ->
  {ok, Res1} = file:read_file('_':join(lists:reverse(Path), "/")),
  {ok, Res2} = case PreAdd of
                 undefined -> {ok, Res1};
                 _ -> '_':apply(PreAdd, [Path, Res1])
               end,
  State#state{result = <<Res/binary, "\r\n", Res2/binary>>}.

parse_opts([{priv_dir, App, Path} | T], State) ->
  {ok, PrivDir, Dir} = case code:priv_dir(App) of
                         Err when element(1, Err) =:= error ->
                           throw(Err);
                         PD ->
                           {ok, PD, Path}
                       end,
  parse_opts(T, State#state{path = [Dir, PrivDir]});
parse_opts([{priv_files, App, Path, Files} | T], State) ->
  parse_opts([{priv_dir, App, Path} | T], State#state{files = Files});
parse_opts([{priv_files, App, Files} | T], State) ->
  parse_opts([{priv_files, App, ".", Files} | T], State);
parse_opts([{suffix, Suffix} | T], State) ->
  parse_opts(T, State#state{
                  suffix = list_to_binary('_':reverse(Suffix)),
                  suflen = byte_size(list_to_binary('_':reverse(Suffix)))
                           });
parse_opts([{path, Path} | T], State) ->
  parse_opts(T, State#state{path = Path});
parse_opts([{recursive, R} | T], State) ->
  parse_opts(T, State#state{recursive = R});
parse_opts([{depth, D} | T], State) ->
  parse_opts(T, State#state{depth = D});
parse_opts([{preadd, PreAdd} | T], State) ->
  parse_opts(T, State#state{preadd = PreAdd});
parse_opts([_Opt | T], State) ->
  parse_opts(T, State);
parse_opts([], State) ->
  State.

%% vim: ft=erlang
