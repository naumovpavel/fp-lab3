%%%-------------------------------------------------------------------
%% @doc fp_lab3 public API
%% @end
%%%-------------------------------------------------------------------

-module(fp_lab3_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fp_lab3_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
