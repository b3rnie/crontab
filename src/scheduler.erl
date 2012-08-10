%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(scheduler).

%%%_* Exports ==========================================================
-export([ add/3
        , del/1
        ]).

%%%_* Includes =========================================================
%%%_* Macros ===========================================================
%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
%%%_ * API -------------------------------------------------------------
add(Name, Spec, {M,F,A} = MFA)
  when erlang:is_atom(Name),
       erlang:is_atom(M),
       erlang:is_atom(F),
       erlang:is_list(A) ->
  case scheduler_time:validate_spec(Spec) of
    ok           -> scheduler_server:add(Name, Spec, MFA);
    {error, Rsn} -> {error, Rsn}
  end.

del(Name)
  when erlang:is_atom(Name) ->
  scheduler_server:del(Name).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
  ok = application:start(scheduler),
  ok = application:stop(scheduler),
  ok = application:start(scheduler),
  ok = application:stop(scheduler).

add_del_test() ->
  ok   = application:start(scheduler),
  Spec = ["*", "*", "*", "*", "*"],
  Mfa  = {m, f, []},
  ok = scheduler:add(foo, Spec, Mfa),
  ok = scheduler:add(bar, Spec, Mfa),
  ok = scheduler:add(baz, Spec, Mfa),
  ok = scheduler:del(foo),
  ok = scheduler:del(bar),
  ok = scheduler:del(baz),
  {error, no_such_task} = scheduler:del(foo),
  {error, no_such_task} = scheduler:del(bar),
  {error, no_such_task} = scheduler:del(baz),
  ok = application:stop(scheduler).

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
