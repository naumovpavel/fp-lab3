-module(fp_lab3).

-export([main/1]).

main(Args) ->
    case parse_args(Args) of
        {ok, Algorithms, Step} ->
            Output = spawn(output, start, []),
            Algos = lists:map(fun(Algo) -> spawn_algo(Algo, Step, Output) end, Algorithms),
            IsSpawnErr =
                fun(A) ->
                   case A of
                       {error, _} ->
                           true;
                       _ ->
                           false
                   end
                end,
            Errors = lists:filter(IsSpawnErr, Algos),
            case length(Errors) > 0 of
                true ->
                    [{_, Reason} | _] = Errors,
                    io:format("Error: ~s~n", [Reason]),
                    usage(),
                    halt(1);
                false ->
                    ok
            end,
            input:read_and_parse(Algos);
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            usage(),
            halt(1)
    end.

spawn_algo(AlgoStr, Step, Output) ->
    case AlgoStr of
        "linear" ->
            spawn(linear, start, [Step, Output]);
        "lagrange" ->
            spawn(lagrange, start, [Step, Output]);
        _ ->
            {error, io_lib:format("Unknown algorithm ~s", [AlgoStr])}
    end.

parse_args(Args) ->
    case {parse_algorithms(Args), parse_step(Args)} of
        {{error, Err}, _} ->
            {error, Err};
        {_, {error, Err}} ->
            {error, Err};
        {{ok, Algs}, {ok, Step}} ->
            {ok, Algs, Step}
    end.

parse_step(Args) ->
    StepStr = get_arg("-step", Args),
    case StepStr of
        undefined ->
            {ok, 1.0};
        StepStr ->
            case number:parse_number(StepStr) of
                {error, _} ->
                    {error, "Invalid value for -step. Must be a number."};
                {ok, Step} ->
                    {ok, Step}
            end
    end.

parse_algorithms(Args) ->
    Algorithm = get_arg("-algorithm", Args),
    case Algorithm of
        undefined ->
            {error, "Missing -algorithm flag."};
        Alg ->
            {ok,
             string:tokens(
                 string:trim(Alg), ",")}
    end.

get_arg(Key, Args) ->
    case lists:dropwhile(fun(Arg) -> Arg =/= Key end, Args) of
        [Key, Value | _] ->
            Value;
        _ ->
            undefined
    end.

% Print usage instructions
usage() ->
    io:format("Usage: fp_lab3 -algorithm <string|string,...,string> -step <number>~n").
