-module(mysql_app).

-behaviour(application).

-export([start/2, stop/1]).

%% application callbacks
start(_Type, _Args) ->
    DbConf = get_app_env(mysql, []),
    mysql_sup:start_link(DbConf).

stop(_State) ->
    stop.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    {ok, App} = application:get_application(),
    case application:get_env(App, Opt) of
        {ok, Val} -> 
            Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error       -> Default
            end
    end.
