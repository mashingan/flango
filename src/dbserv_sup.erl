-module(dbserv_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 60, 3600},
          [{dbserv,
            {dbserv, start_link, []},
            permanent, 1000, worker, [dbserv]}
          ]}}.
