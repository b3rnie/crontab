%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Erlang crontab implementation
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(crontab).

%%%_* Exports ==========================================================
-export([ add/3
        , remove/1
        ]).

%%%_* Includes =========================================================
-include_lib("crontab/include/crontab.hrl").

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
add(Name, Spec, MFA) ->
  add(Name, Spec, MFA, []).

add(Name, Spec, MFA, Options) ->
  case crontab_time:validate_spec(Spec) of
    ok           -> crontab_server:add(Name, Spec, MFA, Options);
    {error, Rsn} -> {error, Rsn}
  end.

remove(Name) ->
  remove(Name, []).

remove(Name, Options) ->
  crontab_server:remove(Name, Options).

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
  {error, spec_format} = crontab:add(foo, bar, {m,f,[]}).

add_remove_test_() ->
  crontab_test:with_crontab(
    fun() ->
	Spec = ["*", "*", "*", "*", "*"],
	MFA  = {crontab_test, execute_funs, [[]]},
	ok   = crontab:add(foo, Spec, MFA),
	ok   = crontab:add(bar, Spec, MFA),
	{error, task_exists} = crontab:add(foo, Spec, MFA),
	ok   = crontab:remove(foo),
	ok   = crontab:remove(bar),
	{error, no_such_task} = crontab:remove(foo),
	{error, no_such_task} = crontab:remove(bar)
    end).

run_test_() ->
  F = fun() ->
	  Daddy = erlang:self(),
	  Ref   = erlang:make_ref(),
	  MFA   = {crontab_test, execute_funs, [[fun() -> Daddy ! Ref end]]},
	  ok    = crontab:add(foo, ["*", "*", "*", "*", "*"], MFA),
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
