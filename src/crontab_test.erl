%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Test support code
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(crontab_test).

%%%_* Exports ==========================================================
-export([ execute_funs/1
	, with_crontab/1
	, waitfor/1
	]).

%%%_* Includes =========================================================
-include_lib("crontab/include/crontab.hrl").

%%%_* Macros ===========================================================
%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
%%%_ * API -------------------------------------------------------------
execute_funs(Fs) ->
  lists:foreach(fun(F) -> F() end, Fs).

with_crontab(F) ->
  fun() ->
      start_app(crontab),
      try F()
      after
	stop_app(crontab)
      end
  end.

waitfor(App) ->
  case whereis(App) of
    undefined -> ok;
    _Pid      -> timer:sleep(1),waitfor(App)
  end.

%%%_ * Internals -------------------------------------------------------
start_app(App) ->
  ok = application:start(App).

stop_app(App) ->
  ok = application:stop(App),
  waitfor(App).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
