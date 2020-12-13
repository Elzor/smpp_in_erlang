-ifndef(SMPP_EXAMPLE_TELEMETRY_HRL).

-define(SMPP_EXAMPLE_TELEMETRY_HRL, true).

%
-define(EXEC_TELEMETRY(Event), ?EXEC_TELEMETRY(Event, #{}, #{})).
-define(EXEC_TELEMETRY(Event, Measurements), ?EXEC_TELEMETRY(Event, Measurements, #{})).
-define(
  EXEC_TELEMETRY(Event, Measurements, Metadata),
  telemetry:execute(Event, Measurements, Metadata)
).

% fake smsc events
-define(FAKE_SMSC_START, [fake_smsc, start]).
-define(FAKE_SMSC_HANDLE_BIND, [fake_smsc, handle, bind]).

% esme events
-define(ESME_START, [esme, start]).
-define(ESME_BOUND, [esme, bound]).
-define(ESME_SUBMIT_SM, [esme, submit_sm]).
-define(ESME_SM_IDS, [esme, sm, ids]).
-define(ESME_SM_ID_DELIVER_REPORT, [esme, sm, id, deliver_report]).
-endif.
