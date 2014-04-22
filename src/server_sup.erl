-module(server_sup).
-behaviour(supervisor).

-define(PORT, 8091).

-export([start_link/0, shell_testing/0, shell_testing/1]).
-export([init/1]).

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    Pid.

shell_testing() ->
    Pid = start_link(),
    shell_testing(Pid).

shell_testing(Pid) ->
    unlink(Pid).

init([]) ->
    {ok, {{one_for_one, 2, 60},
          [{dbserver,
            {dbserv, start_link, []},
            permanent, 1000, worker, [dbserv]},
           {tcpcomm,
            {dbcomm_sup, start_link, [?PORT]},
            permanent, 1000, supervisor, [dbcomm_sup, dbcomm]}
          ]}}.
