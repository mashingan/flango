-module(csv).
-export([csv/1]).

csv(F) ->
    csv(F, read).

csv(F, Var) ->
    {ok, FD} = file:open(F, Var),
    split(read(FD)).

read(FD) ->
    case file:read_line(FD) of
        {ok, Data} ->
            [Data|read(FD)];
        eof -> [];
        {error, Reason} ->
            throw(Reason)
    end.

split(Lines) ->
    lists:map(fun(Line) -> 
                      L = string:strip(Line, both, $\n),
                      re:split(L, "[;]", [{return,list}]) end,
              Lines).
