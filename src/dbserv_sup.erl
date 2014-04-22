-module(dbserv_sup).
-behaviour(supervisor).

-export([start_link/0, shell_testing/0]).
-export([init/1]).

start_link() ->
    %supervisor:start_link(?MODULE, []).
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

shell_testing() ->
    {ok, Pid} = start_link(),
    unlink(Pid).
    

init([]) ->
    {ok, {{one_for_one, 1, 1},
          [{dbserver,
            {dbserv, start_link, []},
            permanent, 1000, worker, [dbserv]}
          ]}}.
