%%%-------------------------------------------------------------------
%% @doc smpp_example public API
%% @end
%%%-------------------------------------------------------------------

-module(smpp_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> smpp_example_sup:start_link().

stop(_State) -> ok.

%% internal functions
