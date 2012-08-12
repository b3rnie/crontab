%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(crontab).

%%%_* Exports ==========================================================
-export([ add/3
        , del/1
        ]).

%%%_* Includes =========================================================
-include_lib("crontab/include/crontab.hrl").

%%%_* Macros ===========================================================
%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
%%%_ * API -------------------------------------------------------------
add(Name, Spec, MFA) ->
  ?hence(erlang:is_atom(Name)),
  ?hence(is_mfa(MFA)),
  case crontab_time:validate_spec(Spec) of
    ok           -> crontab_server:add(Name, Spec, MFA);
    {error, Rsn} -> {error, Rsn}
  end.

del(Name)
  when erlang:is_atom(Name) ->
  crontab_server:del(Name).
%%%_ * Internal --------------------------------------------------------
is_mfa({M,F,A})
  when erlang:is_atom(M),
       erlang:is_atom(F),
       erlang:is_list(A) -> true;
is_mfa(_) -> false.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
  ok = application:start(crontab),
  ok = application:stop(crontab),
  ok = application:start(crontab),
  ok = application:stop(crontab).

bad_spec_test() ->
  {error, spec_format} = crontab:add(foo, bar, {m,f,[]}).

add_del_test() ->
  ok   = application:start(crontab),
  Spec = ["*", "*", "*", "*", "*"],
  Mfa  = {m, f, []},
  ok = crontab:add(foo, Spec, Mfa),
  ok = crontab:add(bar, Spec, Mfa),
  ok = crontab:add(baz, Spec, Mfa),
  ok = crontab:del(foo),
  ok = crontab:del(bar),
  ok = crontab:del(baz),
  {error, no_such_task} = crontab:del(foo),
  {error, no_such_task} = crontab:del(bar),
  {error, no_such_task} = crontab:del(baz),
  ok = application:stop(crontab).

schedule_and_run_test_() ->
  [{setup,
    fun() -> ok = application:start(crontab) end,
    fun(_) -> ok = application:stop(crontab) end,
    [{timeout, 120,
      fun() ->
          Daddy = self(),
          MFA = {crontab_test, execute, [fun() -> Daddy ! done end]},
          ok = crontab:add(test, ["*", "*", "*", "*", "*"], MFA),
          receive done -> ok
          end
      end
     }
    ]
   }
  ].

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
