%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Erlang crontab implementation
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(crontab).

%%%_* Exports ==========================================================
-export([ add/3
	, add/4
        , remove/1
	, remove/2
        ]).

%%%_* Includes =========================================================
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-spec add(Name::atom(), Spec::list(), MFA::mfa()) ->
             whynot(task_exists | no_next_found).
add(Name, Spec, MFA) ->
  add(Name, Spec, MFA, []).

-spec add(Name::atom(), Spec::list(), MFA::mfa(), Options::list()) ->
             whynot(task_exists | no_next_found).
add(Name, Spec, MFA, Options) ->
  case crontab_time:parse_spec(Spec) of
    {ok, PSpec}  -> crontab_server:add(Name, PSpec, MFA, Options);
    {error, Rsn} -> {error, Rsn}
  end.

-spec remove(Name::atom()) -> whynot(no_such_task).
remove(Name) ->
  remove(Name, []).

-spec remove(Name::atom(), Options::list()) -> whynot(no_such_task).
remove(Name, Options) ->
  crontab_server:remove(Name, Options).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
  ok = application:start(crontab),
  ok = application:stop(crontab),
  ok = crontab_test:waitfor(crontab),
  ok = application:start(crontab),
  ok = application:stop(crontab),
  ok = crontab_test:waitfor(crontab).

add_remove_test() ->
  crontab_test:with_crontab(
    fun() ->
	Spec = ['*', '*', '*', '*', '*'],
	MFA  = {crontab_test, execute_funs, [[]]},
	ok   = crontab:add(foo, Spec, MFA),
	ok   = crontab:add(bar, Spec, MFA),
	{error, task_exists} = crontab:add(foo, Spec, MFA),
	ok   = crontab:remove(foo),
	ok   = crontab:remove(bar),
	{error, no_such_task} = crontab:remove(foo),
	{error, no_such_task} = crontab:remove(bar)
    end).

add_bad_spec_test() ->
  crontab_test:with_crontab(
    fun() ->
        Spec = ['*', '*', blah, '*', '*'],
        {error, day} = crontab:add(foo, Spec, {foo, bar, []})
    end).

run_test_() ->
  F = fun() ->
	  Daddy = erlang:self(),
	  Ref   = erlang:make_ref(),
          Fs    = [fun() -> Daddy ! Ref end],
	  MFA   = {crontab_test, execute_funs, [Fs]},
	  ok    = crontab:add(foo, ['*', '*', '*', '*', '*'], MFA),
	  receive Ref -> ok end
      end,
  {timeout, 120, fun() -> crontab_test:with_crontab(F) end}.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
