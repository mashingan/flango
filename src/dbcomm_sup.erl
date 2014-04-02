-module(testserv_sup).
-behaviour(supervisor).

-export([start_link/1, start_socket/0, terminate/0]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
    {ok, Listen} = gen_tcp:listen(Port,[{active,once},binary]),
    spawn_link(fun empty_listener/0),
    {ok, {{simple_one_for_one, 60, 3600},
          [{socket,
            {testserv_genserv, start_link, [Listen]},
            temporary, 1000, worker, [testserv_genserv]}
          ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listener() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.

terminate() ->
    supervisor:terminate(self(), normal).
