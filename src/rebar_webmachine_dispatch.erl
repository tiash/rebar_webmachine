-module(rebar_webmachine_dispatch).

-include("rebar.hrl").
-export([compile/2]). % Rebar dosen't like this abuse of module instances...

compile(Config,_AppFile) ->
  First = lists:flatten([ case file:consult(T) of {ok,D} -> D; _ -> [] end || T <- option(first,Config) ]),
  Last = lists:flatten([ case file:consult(T) of {ok,D} -> D; _ -> [] end || T <- option(last,Config) ]),
  Dispatch = lists:flatten([ dispatcher(D) || D <- option(src,Config) ]),
  Dispatches = First ++ Dispatch ++ Last,
  DispatchFile = option(file,Config),
  ?RES(case Dispatches=/=[] orelse filelib:is_regular(DispatchFile) of
    true ->
      filelib:ensure_dir(DispatchFile),
      File = option(file,Config),
      DispatchText = iolist_to_binary(dispatch_file(Dispatches)),
      case file:read_file(File) of
        {ok,DispatchText} -> ok;
        _ ->
          ?CONSOLE("generated new dispatch.conf~n",[]),
          file:write_file(File,DispatchText)
      end;
    _ -> ok
  end).

dispatcher(Root) ->
  filelib:fold_files(Root,".*",true,fun(File,Disp) ->
    case re:run(File,".*\\.beam$") of
    nomatch -> Disp;
    _ ->
      % ?CONSOLE("checking ~s~n",[File]),
      case beam_lib:chunks(File,[attributes]) of
        {ok,{Module,[{attributes,Attributes}]}} ->
          % ?CONSOLE("  module: ~p~n",[Module]),
          case proplists:get_bool(webmachine_resource,proplists:get_value(behaviour,Attributes,[])) of
            true ->
              Pattern = [ 
                case P of
                  "_" -> '*';
                  [$_,PP] -> list_to_atom(PP);
                  P -> P
                end || P <- string:tokens(atom_to_list(Module),".")],
              % ?CONSOLE("  pattern: ~p~n",[Pattern]),
              Disp ++ [{Pattern,Module,[]}];
            _ -> Disp
          end;
        _Error -> 
          % ?CONSOLE("err: ~p~n",[_Error]),
          Disp
      end
    end end,[]).

dispatch_file(DD) -> ["% vim: ft=erlang\n" | [ io_lib:format("~p.~n",[D]) || D<-DD ]].

  
options(Config) -> rebar_config:get(Config, webmachine_dispatch, []).
option(Opt,Config) ->
    proplists:get_value(Opt, options(Config), default(Opt)).

default(first) -> ["priv/dispatch.first"];
default(last) -> ["priv/dispatch.last"];
default(file) -> "priv/dispatch.conf";
default(src) -> ["src"].

