-module(fake_smsc).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("telemetry.hrl").

%% API

-export(
  [
    start/1,
    init/3,
    terminate/3,
    handle_pdu/2,
    handle_resp/3,
    handle_send_pdu_result/3,
    handle_cast/2,
    handle_socket_error/2,
    handle_socket_closed/1
  ]
).

% elixir modules
-define(mc, 'Elixir.SMPPEX.MC').
-define(pdu, 'Elixir.SMPPEX.Pdu').
-define(factory, 'Elixir.SMPPEX.Pdu.Factory').
-define(errors, 'Elixir.SMPPEX.Pdu.Errors').
-define(session, 'Elixir.SMPPEX.Session').

start(#{port := Port} = Args) ->
  {ok, _ref} =
    ?mc:start({?MODULE, [Args]}, [{transport_opts, [{port, Port}, {max_connections, 100}]}]).

init(_Socket, _Transport, [Args]) ->
  ?EXEC_TELEMETRY(?FAKE_SMSC_START),
  {ok, #{bound => false, last_msg_id => 1, args => Args}}.


handle_resp(_Pdu, _OriginalPdu, State) -> {ok, State}.

handle_pdu(Pdu, State) ->
  case ?pdu:command_name(Pdu) of
    bind_transmitter -> do_handle_bind(Pdu, State);
    bind_receiver -> do_handle_bind(Pdu, State);
    bind_transceiver -> do_handle_bind(Pdu, State);
    submit_sm -> do_handle_submit_sm(Pdu, State);
    unbind -> do_handle_unbind(Pdu, State);
    _ -> {ok, State}
  end.


handle_send_pdu_result(Pdu, _SendPduResult, State) ->
  ?LOG_INFO("send PDU result: ~p", [Pdu]),
  State.


handle_cast(stop, State) -> {stop, normal, State}.

handle_socket_error(Error, State) ->
  ?LOG_INFO("smpp socket error: ~p", [Error]),
  {ok, State}.


handle_socket_closed(State) ->
  ?LOG_INFO("smpp socket closed: ~p", [State]),
  {ok, State}.


terminate(_, _, _State) -> stop.

% ----------------------------------------------------------------------------
% internal
% ----------------------------------------------------------------------------
do_handle_bind(Pdu, #{bound := true} = State) ->
  ?EXEC_TELEMETRY(?FAKE_SMSC_HANDLE_BIND, #{}, #{status => ok}),
  {ok, [bind_resp(Pdu, 'ROK', State)], State};

do_handle_bind(Pdu, #{args := #{system_id := SrvSystemId, password := SrvPass}} = State) ->
  SystemId = ?pdu:field(Pdu, system_id),
  Password = ?pdu:field(Pdu, password),
  case {SystemId, Password} of
    {SrvSystemId, SrvPass} ->
      ?EXEC_TELEMETRY(?FAKE_SMSC_HANDLE_BIND, #{}, #{status => ok}),
      {ok, [bind_resp(Pdu, 'ROK', State)], State#{bound => true}};

    _ ->
      ?EXEC_TELEMETRY(?FAKE_SMSC_HANDLE_BIND, #{}, #{status => error}),
      {ok, [bind_resp(Pdu, 'RALYBND', State)], State}
  end.


do_handle_unbind(Pdu, #{bound := true} = State) ->
  Resp = ?pdu:as_reply_to(?factory:unbind_resp(), Pdu),
  stop(),
  {ok, [Resp], State};

do_handle_unbind(Pdu, State) ->
  Code = ?errors:code_by_name('RINVBNDSTS'),
  Resp = ?pdu:as_reply_to(?factory:unbind_resp(Code), Pdu),
  {ok, [Resp], State}.


do_handle_submit_sm(Pdu, #{bound := true, last_msg_id := LastMsgId} = State) ->
  Code = ?errors:code_by_name('ROK'),
  MsgId = integer_to_binary(LastMsgId + 1),
  Resp1 = ?pdu:as_reply_to(?factory:submit_sm_resp(Code, MsgId), Pdu),
  Resp2 =
    ?pdu:as_reply_to(
      ?factory:delivery_report_for_submit_sm(MsgId, Pdu, <<"dlr message">>, 'DELIVERED'),
      Pdu
    ),
  {ok, [Resp1, Resp2], State#{last_msg_id => LastMsgId + 1}};

do_handle_submit_sm(Pdu, State) ->
  ?debugVal(here),
  Code = ?errors:code_by_name('RINVBNDSTS'),
  Resp = ?pdu:as_reply_to(?factory:submit_sm_resp(Code), Pdu),
  {ok, [Resp], State}.


bind_resp(Pdu, CommandStatus, #{args := #{system_id := SystemId}} = _State) ->
  ?pdu:as_reply_to(
    ?factory:bind_resp(bind_resp_command_id(Pdu), ?errors:code_by_name(CommandStatus), SystemId),
    Pdu
  ).

bind_resp_command_id(Pdu) ->
  % 0x80000000
  2147483648 + ?pdu:command_id(Pdu).


stop() -> ?session:cast(self(), stop).
