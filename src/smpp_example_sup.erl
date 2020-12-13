%%%-------------------------------------------------------------------
%% @doc smpp_example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(smpp_example_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  _ = logger:remove_handler('Elixir.Logger'),
  SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
  case catch fake_smsc:start(smsc_config()) of _any -> ok end,
  ChildSpecs =
    [
      #{
        id => esme,
        start => {esme, start_link, [smsc_config()]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [esme]
      }
    ],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions

smsc_config() -> application:get_env(smpp_example, smsc, #{}).
