-module(mysql_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

-define(POOL, pool).
-define(TIMEOUT, 5000).

start_link(Cfg) ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Cfg]),
    Pools = proplists:get_value(pools, Cfg),
    Host = proplists:get_value(host, Cfg),
    Port = proplists:get_value(port, Cfg),
    User = proplists:get_value(user, Cfg),
    Password = proplists:get_value(password, Cfg),
    Database = proplists:get_value(database, Cfg),
    Encoding = proplists:get_value(encoding, Cfg),
    [mysql:connect(?POOL, Host, Port, User, Password, Database, Encoding, true) 
        || _ <- lists:seq(1, Pools)],
    {ok, Pid}.

%% supervisor callbacks
init([Cfg]) ->
    Host = proplists:get_value(host, Cfg),
    Port = proplists:get_value(port, Cfg),
    User = proplists:get_value(user, Cfg),
    Password = proplists:get_value(password, Cfg),
    Database = proplists:get_value(database, Cfg),
    Encoding = proplists:get_value(encoding, Cfg),
    LogFun = fun (Mod, Line, error, ParamsFun) ->
            {Fmt, Data} = ParamsFun(),
            io:format("## error ~w [~w:~w] " ++ Fmt,
                [erlang:localtime(), Mod, Line | Data]);
        (_Mod, _Line, _Level, _ParamsFun) -> ok
    end,
    ChildSpec = {
        mysql, 
        {mysql, start_link, [?POOL, Host, Port, User, Password, Database, LogFun, Encoding]}, 
        permanent, 
        5000, 
        worker, 
        [mysql]
    },
    {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, [ChildSpec]}}.
