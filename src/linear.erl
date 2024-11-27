-module(linear).

-export([start/2]).

-include_lib("eunit/include/eunit.hrl").

start(Step, Output) ->
    loop([], Step, Output).

loop(Window, Step, Output) ->
    receive
        {X, Y} ->
            case length(Window) > 0 of
                true ->
                    [{X1, Y1}] = lists:nthtail(length(Window) - 1, Window),
                    Output ! {"linear", linear({X1, Y1}, {X, Y}, Step)},
                    loop([{X1, Y1}, {X, Y}], Step, Output);
                false ->
                    loop([{X, Y}], Step, Output)
            end;
        eof ->
            Output ! eof,
            ok;
        _ ->
            loop(Window, Step, Output)
    end.

linear({X1, Y1}, {X2, Y2}, Step) ->
    Steps = round((X2 - X1) / Step),
    Xs = lists:map(fun(I) -> X1 + I * Step end, lists:seq(0, Steps)),
    lists:map(fun(X) -> calc({X1, Y1}, {X2, Y2}, X) end, Xs).

calc({X1, Y1}, {X2, Y2}, X) ->
    Y = (Y1 * (X2 - X) + Y2 * (X - X1)) / (X2 - X1),
    {X, Y}.

lagrange_test() ->
    Res = linear({0, 0.00}, {1.571, 1}, 1.0),
    Xs = lists:map(fun({X, _}) -> X end, Res),
    Ys = lists:map(fun({_, Y}) -> Y end, Res),
    ?assert(["0.00", "1.00", "2.00"] =:= lists:map(fun(Float) -> io_lib:format("~.2f", [Float]) end, Xs)),
    ?assert(["0.00", "0.64", "1.27"] =:= lists:map(fun(Float) -> io_lib:format("~.2f", [Float]) end, Ys)).