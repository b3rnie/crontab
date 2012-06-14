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

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
