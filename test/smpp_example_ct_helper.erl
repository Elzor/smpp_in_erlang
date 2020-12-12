-module(smpp_example_ct_helper).

-export([start/1, name/0]).

%% @doc
%% Start and stop applications and their dependencies.
%% @end

start(Apps) ->
  _ = [do_start(App) || App <- Apps],
  ok.


do_start(App) ->
  case application:start(App) of
    ok -> ok;

    {error, {not_started, Dep}} ->
      do_start(Dep),
      do_start(App)
  end.

%% @doc
%% Return the name of the calling function.
%%
%% With a native VM we always return the current pid because
%% the stacktrace is not as well maintained and may not return
%% the correct function name, especially when there is a mix
%% of normal and native code.
%% @end

name() ->
  case code:is_module_native(kernel) of
    true -> self();
    false -> element(2, hd(tl(element(2, process_info(self(), current_stacktrace)))))
  end.
