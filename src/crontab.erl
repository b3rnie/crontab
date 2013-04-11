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

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-spec add(Name::atom(), Spec::list(), MFA::mfa()) -> ok | {error, _}.
add(Name, Spec, MFA) ->
  add(Name, Spec, MFA, []).

-spec add(Name::atom(), Spec::list(), MFA::mfa(), Options::list()) ->
             ok | {error, _}.
add(Name, Spec, MFA, Options) ->
  case crontab_time:parse_spec(Spec) of
    {ok, PSpec}  -> crontab_server:add(Name, PSpec, MFA, Options);
    {error, Rsn} -> {error, Rsn}
  end.

-spec remove(Name::atom()) -> ok | {error, _}.
remove(Name) ->
  remove(Name, []).

-spec remove(Name::atom(), Options::list()) -> ok | {error, _}.
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

run_test_() ->
  F = fun() ->
	  Daddy = erlang:self(),
	  Ref   = erlang:make_ref(),
	  MFA   = {crontab_test, execute_funs, [[fun() -> Daddy ! Ref end]]},
	  ok    = crontab:add(foo, ['*', '*', '*', '*', '*'], MFA),
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
