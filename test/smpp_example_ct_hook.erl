-module(smpp_example_ct_hook).

-export([init/2]).
-export([post_init_per_suite/4]).
-export([post_end_per_suite/4]).

-include_lib("eunit/include/eunit.hrl").

init(_, _) ->
  application:start(logger),
  init_logger(),
  error_logger:add_report_handler(ct_helper_error_h),
  logger:add_handler(default, logger_std_h, #{config => #{type => standard_io}, level => all}),
  global:register_name(global_io_srv, group_leader()),
  {ok, undefined}.


post_init_per_suite(Suite, _Config, Return, State) -> {before_action(Suite, Return), State}.

post_end_per_suite(Suite, _Config, Return, State) ->
  application:stop(smpp_example),
  {after_action(Suite, Return), State}.

%%=============================================================================
%% helpers
%%=============================================================================

before_action(_Suite, Return) when is_list(Return) ->
  case proplists:get_value(start_apps, Return, undefined) of
    undefined -> pass;

    Apps ->
      [
        begin
          application:load(App),
          [application:set_env(App, Var, Val) || {Var, Val} <- maps:to_list(Config)],
          application:ensure_all_started(App)
        end
        || {App, Config} <- Apps
      ]
  end,
  Return;

before_action(_Suite, Return) -> Return.


after_action(_Suite, Return) when is_list(Return) ->
  case proplists:get_value(start_apps, Return, undefined) of
    undefined -> pass;
    Apps -> [begin application:stop(App) end || {App, _Config} <- Apps]
  end,
  Return;

after_action(_Suite, Return) -> Return.


init_logger() ->
  _ = logger:remove_handler(smpp_example_log),
  Template = [time, " [", level, "] ", pid, " ", mfa, ":", line, " :: ", msg, "\n"],
  HandlerConfig =
    #{
      formatter => {logger_formatter, #{template => Template, single_line => true}},
      config
      =>
      #{type => halt, file => application:get_env(messaging, logfile, "log/smpp_exaple_test.log")},
      filters => [{skip_progress_info, {fun logger_filters:progress/2, stop}}],
      level => debug
    },
  logger:add_handler(smpp_example_log, logger_disk_log_h, HandlerConfig),
  logger:set_primary_config(level, info),
  ok.
