-module(number).

-export([parse_number/1]).

parse_number(Str) ->
    case string:to_float(Str) of
        {error, _} ->
            case string:to_integer(Str) of
                {error, _} ->
                    {error, "Not a valid number"};
                {Int, _} ->
                    {ok, float(Int)}
            end;
        {Float, _} ->
            {ok, Float}
    end.
