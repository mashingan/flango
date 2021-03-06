-module(dbcomm).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, Socket}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(accept, Listen) ->
    {ok, Accept} = gen_tcp:accept(Listen),
    dbcomm_sup:start_socket(),
    {noreply, Accept};
handle_cast(close, Socket) ->
    gen_tcp:close(Socket),
    {stop, normal, Socket}.

handle_info({tcp, _Socket, <<"quit",_/binary>>}, State) ->
    gen_server:cast(self(), close),
    {noreply, State};
handle_info({tcp, Socket, <<"fetch", N/binary>>}, Socket) ->
    inet:setopts(Socket, [{active,once}]),
    lists:foreach(fun(JSON) -> 
                          gen_tcp:send(Socket, JSON),
                          gen_tcp:send(Socket, "\n") end,
                  dbserv:json2sent(binary_to_integer(N))),
    {noreply, Socket};
handle_info({tcp, Socket, Msg}, Socket) ->
    inet:setopts(Socket, [{active, once}]),
    io:format("Msg: ~s~n", [Msg]),
    {noreply, Socket};
handle_info({tcp_closed, Socket}, Socket) ->
    gen_server:cast(self(), close),
    {noreply, Socket}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    io:format("terminate reason: ~p~n", [Reason]).
