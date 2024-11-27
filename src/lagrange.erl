-module(lagrange).

-export([start/2]).

-include_lib("eunit/include/eunit.hrl").

start(Step, Output) ->
    loop([], Step, Output).

loop(Window, Step, Output) ->
    receive
        {X, Y} ->
            case length(Window) > 2 of
                true ->
                    NewWindow = Window ++ [{X, Y}],
                    Points = lists:nthtail(length(NewWindow) - 4, NewWindow),
                    Output ! {"lagrange", lagrange(Step, Points)},
                    loop(Points, Step, Output);
                false ->
                    loop(Window ++ [{X, Y}], Step, Output)
            end;
        eof ->
            Output ! eof,
            ok;
        _ ->
            loop(Window, Step, Output)
    end.

lagrange(Step, Points) ->
    [{X0, _} | _] = Points,
    {Xn, _} = lists:last(Points),
    Steps = round((Xn - X0) / Step),
    Xs = lists:map(fun(I) -> X0 + I * Step end, lists:seq(0, Steps)),
    lists:map(fun(X) -> calc(Points, X) end, Xs).

calc(Points, X) ->
    Li = fun(Xi) ->
            NonEqual = lists:filter(fun({Xj, _}) -> Xj =/= Xi end, Points),
            Top = lists:foldl(fun({Xj, _}, Acc) -> Acc * (X - Xj) end, 1, NonEqual),
            Bottom = lists:foldl(fun({Xj, _}, Acc) -> Acc * (Xi - Xj) end, 1, NonEqual),
            Top / Bottom
         end,

    Y = lists:foldl(fun({Xi, Yi}, Acc) -> Acc + Yi * Li(Xi) end, 0, Points),
    {X, Y}.

lagrange_test() ->
    Res = lagrange(1.0, [{0.0,0.0},{1.571,1.0},{3.142,0.0},{4.712,-1.0}]),
    Xs = lists:map(fun({X, _}) -> X end, Res),
    Ys = lists:map(fun({_, Y}) -> Y end, Res),
    ?assert(["0.00", "1.00", "2.00", "3.00", "4.00", "5.00"] =:= lists:map(fun(Float) -> io_lib:format("~.2f", [Float]) end, Xs)),
    ?assert(["0.00", "0.97", "0.84", "0.12", "-0.67", "-1.03"] =:= lists:map(fun(Float) -> io_lib:format("~.2f", [Float]) end, Ys)).
