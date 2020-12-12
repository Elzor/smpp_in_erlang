-module(common_SUITE).

-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    {group, app_checks}
  ].

groups() ->
  [
    {app_checks,
      [parallel, shuffle],
      [module_load, start_stop]}
  ].


%% =============================================================================
%% init
%% =============================================================================
init_per_suite(_) ->
  application:stop(smpp_example),
  [].

init_per_group(_Group, Config) ->
  [{init, true} | Config].


%% =============================================================================
%% end
%% =============================================================================
end_per_group(_Group, _Config) ->
  ok.

end_per_suite(_) ->
  ok.


%% =============================================================================
%% group: common_app_checks
%% =============================================================================
module_load(_) ->
  {module, smpp_example_sup} = code:load_file(smpp_example_sup).

start_stop(_) ->
  ok = application:start(smpp_example),
  timer:sleep(150),
  {error, {already_started, smpp_example}} = application:start(smpp_example),
  ok = application:stop(smpp_example).
