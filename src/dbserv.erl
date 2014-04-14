-module(dbserv).
-behaviour(gen_server).

%% external API identifier %%
-export([add/1, remove/1, listall/0, read/1, read/2]).

%% application API, only invoked once %%
-export([do_once/0]).

%% gen_server API handling %%
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-include_lib("stdlib/include/qlc.hrl").

-record(dbquest, {id, question, choices, answer, file}).

add(L) ->
    gen_server:call({add, L}).

remove(Id) ->
    gen_server:call({remove, Id}).

listall() ->
    gen_server:cast(self(), list).

read(Id) ->
    gen_server:cast(self(), {read, Id}).

read(Id, Field) ->
    gen_server:cast(self(), {read, Id, Field}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    start(),
    {ok, []}. % Actually state but using empty list

%%handle_call(_Request, _From, State) -> {reply, Reply,  State}.
handle_call({add, L}, _From, State) ->
    Reply = case add_question(L) of
                {atomic, _Val} ->
                    ok;
                _ ->
                    "Incorrect argument, use list"
            end,
    {reply, Reply,  State};
handle_call({remove, Id}, _From, State) ->
    Reply = case remove_question(Id) of
                {atomic, _Val} ->
                    ok;
                _ ->
                    "Incorrect argument, use Id"
            end,
    {reply, Reply, State}.

%%handle_cast(_Msg, State) -> {noreply, State}.
handle_cast(list, State) ->
    Reply = read_db(),
    {reply, Reply, State};
handle_cast({read, Id}, State) ->
    Reply = querydb(Id),
    {reply, Reply, State};
handle_cast({read, Id, Field}, State) ->
    Reply = querydb(Id, Field),
    {reply, Reply, State}.

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
                                   record_info(fields, dbquest)}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([dbquest], 10000).

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
    mnesia:transaction(F).

remove_question(Id) ->
    F = fun() -> mnesia:delete({dbquest, Id}) end,
    mnesia:transaction(F).

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