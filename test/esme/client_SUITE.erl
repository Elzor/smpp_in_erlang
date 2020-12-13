-module(client_SUITE).

-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("telemetry.hrl").

all() ->
  [
    {group, commands}
  ].

groups() ->
  [
    {commands,
      [parallel, shuffle],
      [send_sm]}
  ].


%% =============================================================================
%% init
%% =============================================================================
init_per_suite(_) ->
  application:ensure_all_started(smpp_example),
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
%% group: commands
%% =============================================================================
send_sm(_) ->
  test_helpers:subscribe_on_events([
    ?ESME_SUBMIT_SM,
    ?ESME_SM_IDS,
    ?ESME_SM_ID_DELIVER_REPORT
  ]),

  % send SMS
  FromMsisdn = <<"901">>,
  Ref = <<"test_submit1">>,
  Message = #{ref => Ref, msisdn => <<"79613322188">>, parts => [<<"payload1">>], until => erlang:system_time(seconds) + 86400},
  timer:sleep(100),
  ok = esme:send_sm(FromMsisdn, Message),
  {_,#{ref := Ref, status := ok}} = test_helpers:receive_event_data(?ESME_SUBMIT_SM, 5000),

  % deliver_sm received
  {_,#{sm_ids := Ids}} = test_helpers:receive_event_data(?ESME_SM_IDS, 5000),
  1 = length(Ids),
  {_,#{sm_id := Id}} = test_helpers:receive_event_data(?ESME_SM_ID_DELIVER_REPORT, 5000),
  true = lists:member(Id, Ids),
  ok.