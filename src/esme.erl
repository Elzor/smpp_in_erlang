-module(esme).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("telemetry.hrl").

%% API

-export(
  [
    start_link/1,
    init/3,
    handle_info/2,
    handle_call/3,
    terminate/3,
    handle_resp/3,
    handle_resp_timeout/2,
    handle_unparsed_pdu/3,
    handle_socket_error/2,
    handle_socket_closed/1,
    handle_send_pdu_result/3,
    handle_pdu/2,
    send_sm/2
  ]
).

% bits
-define(b0, 0 : 1).
-define(b1, 1 : 1).

% elixir modules
-define(esme, 'Elixir.SMPPEX.ESME').
-define(pdu_factory, 'Elixir.SMPPEX.Pdu.Factory').
-define(pdu, 'Elixir.SMPPEX.Pdu').

%%--------------------------------------------------------------------
%% @doc
%% Starts link
%% @end
%%--------------------------------------------------------------------

start_link(#{host := Host, port := Port} = Opts) -> ?esme:start_link(Host, Port, {?MODULE, [Opts]}).

send_sm(FromMsisdn, Message) -> gen_server:call(whereis(test_esme), {send_sm, FromMsisdn, Message}).

%%%===================================================================
%%% Link callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the link
%% @end
%%--------------------------------------------------------------------

init(_, _, [Opts]) ->
  ?EXEC_TELEMETRY(?ESME_START),
  register(test_esme, self()),
  erlang:send_after(100, self(), bind),
  {ok, #{opts => Opts, delivery_wait_list => #{}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling calls
%% @end
%%--------------------------------------------------------------------

handle_call(
  {send_sm, FromMsisdn, #{ref := Ref, msisdn := Msisdn, parts := Parts, until := Until} = Message},
  _From,
  #{delivery_wait_list := DeliveryWaitList} = State
) ->
  Now = erlang:system_time(seconds),
  case Until >= Now + send_msg_minimal_timeout() of
    true ->
      ValidPeriod = Until - Now,
      SubmitSmList = [submit_sm(FromMsisdn, Msisdn, Part, ValidPeriod) || Part <- Parts],
      SumbitSmRefs = [SubmitSmRef || #{ref := SubmitSmRef} <- SubmitSmList],
      ?EXEC_TELEMETRY(?ESME_SUBMIT_SM, #{}, #{status => ok, ref => Ref, proto_refs => SumbitSmRefs}),
      {
        reply,
        ok,
        SubmitSmList,
        State#{
          delivery_wait_list
          =>
          DeliveryWaitList#{
            Ref => #{message => Message, sm_refs => SumbitSmRefs, sm_ids => [], created_ts => Now}
          }
        }
      };

    false -> {reply, {error, msg_not_found}, State}
  end;

handle_call({handle_test_pdu, Pdu}, _From, State) ->
  process_pdu_command(Pdu, State),
  {reply, ok, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all messages
%% @end
%%--------------------------------------------------------------------

handle_info(bind, #{opts := #{password := Pass, system_id := SystemId}} = State) ->
  {noreply, [?pdu_factory:bind_transceiver(SystemId, Pass)], State};

handle_info(_Msg, State) -> {noreply, [], State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling smpp resp
%% @end
%%--------------------------------------------------------------------

handle_resp(Pdu, #{ref := OrigRef} = _OrigPdu, State) ->
  NewState =
    case ?pdu:command_name(Pdu) of
      bind_transceiver_resp ->
        ?EXEC_TELEMETRY(?ESME_BOUND, #{}, #{mode => transceiver}),
        State;

      bind_receiver_resp ->
        ?EXEC_TELEMETRY(?ESME_BOUND, #{}, #{mode => receiver}),
        State;

      submit_sm_resp ->
        MsgId = ?pdu:mandatory_field(Pdu, message_id),
        ?LOG_INFO("wait for id: ~p (~p)", [MsgId, self()]),
        NewState1 = apply_msg_id(State, MsgId, OrigRef),
        NewState1;

      SomeCmd ->
        ?LOG_WARNING("unhandled pdu: ~p", [SomeCmd]),
        State
    end,
  {ok, NewState}.


handle_resp_timeout(_Pdu, State) ->
  ?LOG_WARNING("send pdus timeout"),
  {ok, State}.


handle_send_pdu_result(_Pdu, ok, State) -> State;

handle_send_pdu_result(Pdu, SendPduResult, State) ->
  ?LOG_WARNING("pdu send status: ~p", [{Pdu, SendPduResult}]),
  State.


handle_pdu(Pdu, State) ->
  {SubmitPdus, NewState} = process_pdu_command(Pdu, State),
  {ok, SubmitPdus, NewState}.


process_pdu_command(Pdu, State) ->
  case ?pdu:command_name(Pdu) of
    deliver_sm ->
      %% Delivery receipts and MO messages
      try
        EsmClass = ?pdu:mandatory_field(Pdu, esm_class),
        case <<EsmClass>> of
          <<_Head : 2, ?b0, ?b0, ?b0, ?b1, _Tail : 2>> -> handle_delivery_receipt(Pdu, State);
          <<_Head : 2, ?b0, ?b0, ?b0, ?b0, _Tail : 2>> -> handle_standart_message(Pdu, State);

          _Else ->
            ?LOG_ERROR("unknown deliver_sm: ~p", [_Else]),
            {[], State}
        end
      catch
        E:R : Trace ->
          ?LOG_ERROR("deliver_sm parse crash: ~p", [{E, R, Trace}]),
          {[], State}
      end;

    Else ->
      ?LOG_ERROR("wrong SMSC message: ~p", [Else]),
      {[], State}
  end.


handle_delivery_receipt(Pdu, #{opts := #{system_id := _SystemId}} = State) ->
  SmsId = ?pdu:field(Pdu, receipted_message_id),
  ?EXEC_TELEMETRY(?ESME_SM_ID_DELIVER_REPORT, #{}, #{sm_id => SmsId}),
  ?LOG_INFO("handle delivery id: ~p (~p)", [SmsId, self()]),
  ReplyPdu = ?pdu:as_reply_to(deliver_sm_resp(SmsId), Pdu),
  {[ReplyPdu], State}.


handle_standart_message(Pdu, State) ->
  ReplyPdus =
    case ?pdu:field(Pdu, short_message) of
      Message when is_binary(Message), size(Message) > 0 ->
        MsgId = ?pdu:mandatory_field(Pdu, message_id),
        Msisdn = ?pdu:field(Pdu, source_addr),
        ?LOG_INFO("handle mo from: ~p", [Msisdn]),
        %% MO сообщение принято
        ReplyPdu = ?pdu:as_reply_to(deliver_sm_resp(MsgId), Pdu),
        [ReplyPdu];

      _Else ->
        ?LOG_ERROR("wrong mo message: ~p", [_Else]),
        []
    end,
  {ReplyPdus, State}.


handle_unparsed_pdu(Pdu, Error, State) ->
  ?LOG_WARNING("unparsed pdu: ~p error: ~p", [Pdu, Error]),
  {ok, State}.


handle_socket_error(Error, State) ->
  ?LOG_ERROR("smpp socket error: ~p", [Error]),
  {network_issue, State}.


handle_socket_closed(State) ->
  ?LOG_ERROR("smpp socket closed: ~p", [State]),
  {socket_closed, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a link when it is about to
%% terminate.
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _, _State) ->
  timer:sleep(100),
  stop.

%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_msg_id(#{delivery_wait_list := DeliveryWaitList} = State, MsgId, OrigRef) ->
  State#{delivery_wait_list => store_msg_id(DeliveryWaitList, OrigRef, MsgId)}.

store_msg_id(DeliveryWaitList, OrigRef, MsgId) ->
  maps:from_list(
    lists:foldr(
      fun
        ({CmdId, #{sm_refs := SmRefs, sm_ids := SmIds} = CmdOpts} = E, A) ->
          case lists:member(OrigRef, SmRefs) of
            true ->
              NewSmIds = [MsgId | SmIds],
              case length(SmRefs) == length(NewSmIds) of
                true ->
                  % all parts resolved
                  ?EXEC_TELEMETRY(?ESME_SM_IDS, #{}, #{sm_ids => NewSmIds}),
                  proplists:delete(CmdId, A);

                false -> [{CmdId, CmdOpts#{sm_ids => NewSmIds}} | A]
              end;

            false -> [E | A]
          end
      end,
      [],
      maps:to_list(DeliveryWaitList)
    )
  ).


submit_sm(SourceMsisdn, DestMsisdn, Message, Ttl) ->
  {ok, CommandId} = 'Elixir.SMPPEX.Protocol.CommandNames':id_by_name(submit_sm),
  {D, {H, M, S}} = calendar:seconds_to_daystime(Ttl),
  VP = lists:flatten(io_lib:format("0000~2..0w~2..0w~2..0w~2..0w000R", [D, H, M, S])),
  ?pdu:new(
    CommandId,
    #{
      source_addr => SourceMsisdn,
      source_addr_ton => 1,
      source_addr_npi => 1,
      destination_addr => DestMsisdn,
      dest_addr_ton => 1,
      dest_addr_npi => 1,
      short_message => Message,
      data_coding => 246,
      protocol_id => 127,
      %% For concatenaded messages
      esm_class => 64,
      registered_delivery => 1,
      validity_period => list_to_binary(VP)
    }
  ).


deliver_sm_resp(MessageId) ->
  {ok, CommandId} = 'Elixir.SMPPEX.Protocol.CommandNames':id_by_name(deliver_sm_resp),
  CommandStatus = 0,
  SeqNumber = 0,
  ?pdu:new({CommandId, CommandStatus, SeqNumber}, #{message_id => MessageId}, #{}).


send_msg_minimal_timeout() -> application:get_env(smpp_example, send_msg_minimal_timeout_sec, 300).
