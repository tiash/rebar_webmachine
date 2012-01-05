-module(rebar_webmachine_rest).

% -on_load(on_load/0).
-export([compile/2]).

-include("rebar.hrl").

-define(CODEGEN,(list_to_atom(?MODULE_STRING++".erltl"))).

on_load() ->
  case code:ensure_loaded(?CODEGEN) of
    {module,_} ->
      %io:format("~p~n",[?CODEGEN:module_info()]),
      ok;
    _ ->
      File = filename:rootname(?FILE)++".erltl",
      % io:format("~s~n",[File]),
      case erltl2:compile(File,?CODEGEN,[binary,return,load]) of
        {ok,_,Code,_} ->
          _Res = code:load_binary(?CODEGEN,File,Code),
          % io:format("~p~n",[_Res]),
          on_load();
        {error,enoent} -> ok
      end
  end,
  ok.


compile(Config, _AppFile) ->
    % io:format("~s:~b ~~ ~s:compile(~p,~p).~n",[?FILE,?LINE,?MODULE,Config,_AppFile]),
    on_load(),
    RestOpts = rest_opts(Config),
    ?RES(
    rebar_base_compiler:run(Config, [],
                            option(src_dir, RestOpts),
                            option(source_ext, RestOpts),
                            option(out_dir, RestOpts),
                            option(out_ext, RestOpts),
                            fun compile_tl/3, [{check_last_mod, false}])).

rest_opts(Config) ->
    rebar_config:get(Config, webmachine.rest, []).

option(Opt, RestOpts) ->
    proplists:get_value(Opt, RestOpts, default(Opt)).

default(src_dir) -> "ebin";
default(out_dir)  -> "src";
default(source_ext) -> ".beam";
default(out_ext) -> "/_.erl".

compile_tl(Source, Target, Config) ->
            case needs_compile(Source, Target, Config) of
                true ->
                    do_compile(Source, Target, Config);
                false ->
                    skipped
            end.

do_compile(Module, Target, _Config) when is_atom(Module) ->
  % io:format("~p: ~p -> ~s~n",[?LINE,Module,Target]),
  Code = (catch iolist_to_binary((?CODEGEN:code(Module)))),
  % io:format("~p: ~p -> ~s~n%%%%%%%%%%%%%%%%%%~s~n%%%%%%%%%%%%%%%%%%~n",[?LINE,Module,Target,Code]),
  case catch file:read_file(Target) of
    {ok,Code} ->
      % io:format("~p: ~p -> ~s~n",[?LINE,Module,Target]),
      ok;
    _ ->
      filelib:ensure_dir(Target),
      Res = (catch file:write_file(Target,Code)),
      % io:format("~p: ~p -> ~s~n    ~p~n",[?LINE,Module,Target,Res]),
      Res
  end;
do_compile(Source, Target, Config) ->
  % io:format("~p: ~s -> ~s~n",[?LINE,Source,Target]),
  case file:read_file(Source) of
    {ok,Beam} ->
      case catch beam_lib:chunks(Beam,[attributes]) of
        {ok,{Module,_}} ->
          % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,Module]),
          case catch code:ensure_loaded(Module) of
            {module,_} ->
              % io:format("~p: ~s -> ~s~n",[?LINE,Source,Target]),
              do_compile(Module,Target,Config);
            {error,_E} ->
              % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,_E]),
              case catch code:load_binary(Module,Source,Beam) of
                {module,Module} ->
                  % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,Module]),
                  Res = do_compile(Module,Target,Config),
                  % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,Res]),
                  % code:delete(Module),
                  % code:purge(Module),
                  Res;
                Err={error,_} ->
          % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,Err]),
          Err;
        Err=_ ->
          % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,Err]),
          Err
      end;

        Err=_ ->
          % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,Err]),
          Err
          end;
        Err={error,_,_} ->
          % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,Err]),
          Err;
        Err=_ ->
          % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,Err]),
          Err
      end;
    Err=_ ->
      % io:format("~p: ~s -> ~s~n    ~p~n",[?LINE,Source,Target,Err]),
      Err
  end.

needs_compile(Source, Target, _Config) ->
    % io:format("~s -> ~s~n",[Source,Target]),
    LM = filelib:last_modified(Target),
    LM < filelib:last_modified(Source) andalso
    (case beam_lib:chunks(Source,[attributes]) of
        {ok,{_Module,[{attributes,Attrs}]}} -> 
          proplists:get_bool(rest_resource,proplists:get_value(behaviour,Attrs,[]));
        _ -> false
    end).




