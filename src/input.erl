-module(input).

-export([read_and_parse/1]).

read_and_parse(AlgoPids) ->
    io:format("Enter lines with two floats separated by space (Ctrl+D to exit):~n"),
    loop(AlgoPids).

loop(AlgoPids) ->
    case io:get_line("") of
        eof ->
            lists:foreach(fun(AlgoPid) -> AlgoPid ! eof end, AlgoPids),
            io:format("Goodbye!~n"),
            ok;
        {error, _} ->
            io:format("Error reading input. Exiting.~n"),
            {error, "Error reading input"};
        Line ->
            case parse_line(Line) of
                {ok, X, Y} ->
                    lists:foreach(fun(AlgoPid) -> AlgoPid ! {X, Y} end, AlgoPids),
                    loop(AlgoPids);
                {error, Reason} ->
                    io:format("Error: ~s~n", [Reason]),
                    loop(AlgoPids)
            end
    end.

parse_line(Line) ->
    Tokens =
        string:tokens(
            string:trim(Line), " "),
    case Tokens of
        [XStr, YStr] ->
            case {number:parse_number(XStr), number:parse_number(YStr)} of
                {{ok, X}, {ok, Y}} ->
                    {ok, X, Y};
                _ ->
                    {error, "Invalid numbers."}
            end;
        _ ->
            {error, "Invalid input."}
    end.
