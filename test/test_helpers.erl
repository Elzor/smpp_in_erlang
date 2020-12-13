-module(test_helpers).

-export([subscribe_on_events/1, receive_event/2, receive_event_data/2]).

-include_lib("eunit/include/eunit.hrl").

subscribe_on_events(Events) ->
  TestProcess = self(),
  ok =
    telemetry:attach_many(
      get_random_string(32, "qwertyQWERTY1234567890"),
      Events,
      fun (Event, Meas, Meta, _Config) -> TestProcess ! {event, Event, Meas, Meta} end,
      []
    ),
  ok.


receive_event(Event, Timeout) -> receive {event, Event, _, _} -> true after Timeout -> timeout end.

receive_event_data(Event, Timeout) ->
  receive {event, Event, Meas, Meta} -> {Meas, Meta} after Timeout -> timeout end.

get_random_string(Length, AllowedChars) ->
  lists:foldl(
    fun (_, Acc) -> [lists:nth(rand:uniform(length(AllowedChars)), AllowedChars)] ++ Acc end,
    [],
    lists:seq(1, Length)
  ).
