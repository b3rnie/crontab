%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Erlang crontab implementation
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(crontab).

%%%_* Exports ==========================================================
-export([ schedule/3
        , unschedule/1
	, all_tasks/0
        ]).

%%%_* Includes =========================================================
-include_lib("crontab/include/crontab.hrl").

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
schedule(Name, Spec, MFA) ->
  schedule(Name, Spec, MFA, []).

schedule(Name, Spec, MFA, Options) ->
  case crontab_time:validate_spec(Spec) of
    ok           -> crontab_server:schedule(Name, Spec, MFA, Options);
    {error, Rsn} -> {error, Rsn}
  end.

unschedule(Name) ->
  unschedule(Name, []).

unschedule(Name, Options) ->
  crontab_server:unschedule(Name, Options).

all_tasks() ->
  crontab_server:all_tasks().

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test_() ->
  fun() ->
      ok = application:start(crontab),
      ok = application:stop(crontab),
      ok = crontab_test:waitfor(crontab),
      ok = application:start(crontab),
      ok = application:stop(crontab),
      ok = crontab_test:waitfor(crontab)
  end.

bad_spec_test() ->
  {error, spec_format} = crontab:schedule(foo, bar, {m,f,[]}).

schedule_unschedule_test_() ->
  crontab_test:with_crontab(
    fun() ->
	Spec = ["*", "*", "*", "*", "*"],
	MFA  = {crontab_test, execute_funs, [[]]},
	ok   = crontab:schedule(foo, Spec, MFA),
	ok   = crontab:schedule(bar, Spec, MFA),
	ok   = crontab:unschedule(foo),
	ok   = crontab:unschedule(bar),
	{error, no_such_task} = crontab:unschedule(foo),
	{error, no_such_task} = crontab:unschedule(bar)
    end).

run_test_() ->
  F = fun() ->
	  Daddy = erlang:self(),
	  Ref   = erlang:make_ref(),
	  MFA   = {crontab_test, execute_funs, [[fun() -> Daddy ! Ref end]]},
	  ok    = crontab:schedule(foo, ["*", "*", "*", "*", "*"], MFA),
	  receive Ref -> ok end
      end,
  {timeout, 120, crontab_test:with_crontab(F)}.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
