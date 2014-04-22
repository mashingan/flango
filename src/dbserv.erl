-module(dbserv).
-behaviour(gen_server).

%% external API identifier %%
-export([add/1, remove/1, listall/0, read/1, read/2, reset/0,
        fetch/1, r2plist/1]).

%% application API, only invoked once %%
-export([do_once/0]).

%% gen_server API handling %%
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-include_lib("stdlib/include/qlc.hrl").

-record(dbquest, {id, question, choices, answer, file}).

add(L) ->
    gen_server:call(?MODULE, {add, L}).

remove(Id) ->
    gen_server:call(?MODULE, {remove, Id}).

listall() ->
    gen_server:call(?MODULE, list).

fetch(N) ->
    gen_server:call(?MODULE, {fetch, N}).

read(Id) ->
    gen_server:call(?MODULE, {read, Id}).

read(Id, Field) ->
    gen_server:call(?MODULE, {read, Id, Field}).

start_link() ->
    %gen_server:start_link(?MODULE, [], []).
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    start(),
    {ok, lists:map(fun(X) -> X#dbquest.id end, read_db())}.

handle_call({add, L}, _From, IdList) ->
    {ok, NewContent} = add_question(L),
    IdsList = [NewContent#dbquest.id | IdList],
    {reply, ok, IdsList};
handle_call({remove, Id}, _From, State) ->
    Reply = case remove_question(Id) of
                {atomic, _Val} ->
                    ok;
                _ ->
                    "Incorrect argument, use Id"
            end,
    {reply, Reply, lists:delete(Id, State)};
handle_call({fetch, N}, _From, State) ->
    Reply = fetching(N, State),
    {reply, Reply, State};
handle_call(list, _From, State) ->
    Reply = read_db(),
    {reply, Reply, State};
handle_call({read, Id}, _From, State) ->
    [Reply] = querydb(Id),
    {reply, Reply, State};
handle_call({read, Id, Field}, _From, State) ->
    [Reply] = querydb(Id, Field),
    {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%== Internal API ==%%
do_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(dbquest, [{attributes,
                                   record_info(fields, dbquest)},
                                  {type, set},
                                  {disc_copies, [node()]}
                                 ]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([dbquest], 10000).

reset() ->
    mnesia:clear_table(dbquest),
    lists:foreach(fun(X) -> add(X) end, csv:csv("test/test.csv")).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

add_question(L) ->
    [Q, C1, C2, C3, C4, A, File] = L,
    Row = #dbquest{id=uuid:to_string(uuid:uuid4()),
                   question=Q,
                   choices=[C1,C2,C3,C4],
                   answer=A,
                   file=File},
    F = fun() -> mnesia:write(Row) end,
    {atomic, _Val} = mnesia:transaction(F),
    {ok, Row}.

remove_question(Id) ->
    F = fun() -> mnesia:delete({dbquest, Id}) end,
    mnesia:transaction(F).

fetching(N, IdsList) ->
    fetching(N, length(IdsList), IdsList, []).

fetching(N, M, IdsList, Res) when N > 0 ->
    NewFetch = lists:nth(random:uniform(M), IdsList),
    case lists:member(NewFetch, Res) of
        true ->
            fetching(N, M, IdsList, Res);
        false ->
            fetching(N-1, M, IdsList, [NewFetch|Res])
    end;
fetching(0, _, _, Res) ->
    Res.

read_db() ->
    do(qlc:q([X || X <- mnesia:table(dbquest)])).

querydb(Id) ->
    do(qlc:q([X || X <- mnesia:table(dbquest),
                   X#dbquest.id =:= Id])).

querydb(Id, question) ->
    do(qlc:q([X#dbquest.question || X <- mnesia:table(dbquest),
                                    X#dbquest.id =:= Id]));
querydb(Id, answer) ->
    do(qlc:q([X#dbquest.answer || X <- mnesia:table(dbquest),
                                  X#dbquest.id =:= Id]));
querydb(Id, file) ->
    do(qlc:q([X#dbquest.file || X <- mnesia:table(dbquest),
                                X#dbquest.id =:= Id])).

r2plist(Rec) ->
    lists:zip(record_info(fields, dbquest),
              lists:map(fun r2helper/1,
                        tl(tuple_to_list(Rec)))).

r2helper(X) when is_list(X)  ->
    case io_lib:printable_list(X) of
        true  -> list_to_binary(X);
        false -> lists:map(fun r2helper/1, X)
    end;
r2helper(X) -> X.
