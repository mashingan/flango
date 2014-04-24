-module(server_sup).
-behaviour(supervisor).

-export([main/1, start_link/1, shell_testing/1]).
-export([init/1]).

main([Arg]) ->
    Port = list_to_integer(atom_to_list(Arg)),
    start_link(Port).

start_link(Port) ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]),
    Pid.

shell_testing(Port) ->
    Pid = start_link([Port]),
    shell_testing(Pid).

init([Port]) ->
    {ok, {{one_for_one, 2, 60},
          [{dbserver,
            {dbserv, start_link, []},
            permanent, 1000, worker, [dbserv]},
           {tcpcomm,
            {dbcomm_sup, start_link, [Port]},
            permanent, 1000, supervisor, [dbcomm_sup, dbcomm]}
          ]}}.
