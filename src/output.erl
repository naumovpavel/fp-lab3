-module(output).

-export([start/0]).

start() ->
    loop().

loop() ->
    receive
        {Method, Res} ->
            io:format("~s:~n", [Method]),
            Xs = lists:map(fun({X, _}) -> X end, Res),
            Ys = lists:map(fun({_, Y}) -> Y end, Res),
            print_list(Xs),
            print_list(Ys),
            loop();
        eof ->
            ok;
        _ ->
            loop()
    end.

print_list(List) ->
    Formatted = lists:map(fun(Float) -> io_lib:format("~.2f", [Float]) end, List),
    io:format("~s~n", [string:join(Formatted, "\t")]).
