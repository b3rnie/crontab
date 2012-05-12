%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc parse spec, calculate next runtime
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(scheduler_spec).
-compile(export_all).
-compile(no_auto_import, [now/1]).

%%%_* Exports ==========================================================
-export([ parse/1
        , next/2
        ]).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(spec, { min
              , hour
              , dom
              , month
              , dow
              }).

%%%_ * API -------------------------------------------------------------
parse([_,_,_,_,_] = Spec) ->
  case first_fail(fun validate/1, lists:zip(units(), Spec)) of
    ok           -> {ok, Spec};
    {error, Rsn} -> {error, Rsn}
  end;
parse(_Spec) ->
  {error, spec_format}.

%%%_ * Internal --------------------------------------------------------
first_fail(F, [H|T]) ->
  case F(H) of
    ok           -> first_fail(F, T);
    {error, Rsn} -> {error, Rsn}
  end;
first_fail(_F, []) -> ok.

validate({_Unit, "*"}) -> ok;
validate({year, Year}) ->
  case is_integer_range(Year, 0, inf) of
    true  -> ok;
    false -> {error, spec_year}
  end;
validate({month, Month}) ->
  case is_integer_range(Month, 1, 12) of
    true  -> ok;
    false -> {error, spec_month}
  end;
validate({day, monday})    -> ok;
validate({day, tuesday})   -> ok;
validate({day, wednesday}) -> ok;
validate({day, thursday})  -> ok;
validate({day, friday})    -> ok;
validate({day, saturday})  -> ok;
validate({day, sunday})    -> ok;
validate({day, Day}) ->
  case is_integer_range(Day, 1, 31) of
    true  -> ok;
    false -> {error, spec_day}
  end;
validate({hour, Hr}) ->
  case is_integer_range(Hr, 0, 23) of
    true  -> ok;
    false -> {error, spec_hr}
  end;
validate({minute, Min}) ->
  case is_integer_range(Min, 0, 59) of
    true  -> ok;
    false -> {error, spec_minute}
  end.

is_integer_range(N, _Start, _End) when not erlang:is_integer(N) -> false;
is_integer_range(N, inf,    End) when N =< End                  -> true;
is_integer_range(N, Start, inf)  when N >= Start                -> true;
is_integer_range(N, Start, End) -> N >= Start andalso N =< End.

units() -> [year, month, day, hour, minute].

%% @doc search for next runtime
%%
next_run(Spec, Now) ->
  Dates = [Date || [Y,M,D,_,_] = Date <- expand(Spec, Now),
                   calendar:valid_date(Y,M,D)],
  lists:dropwhile(fun(Date) -> Date < Now end, lists:sort(Dates)).

expand(Spec, Now) ->
  Start = next(min, fetch(min, Spec), fetch(min, Now)),
  expand([hour, day, month, year], Spec, Now, [Start]).

expand([Unit|Units], Spec, Now, Acc) ->
  Nexts = next(Unit, fetch(Unit, Spec), fetch(Unit, Now)),
  expand(Units, Spec, Now, [[N|X] || X <- Acc, N <- Nexts]);
expand([], _Spec, _Now, Acc) -> Acc.

next(min,   "*", 59) -> [0];
next(min,   "*",  M) -> [0, M+1];
next(min,   M,    _) -> [M];
next(hour,  "*", 23) -> [0, 23];
next(hour,  "*",  H) -> [0, H, H+1];
next(hour,  H,    _) -> [H];
next(day,   "*", 28) -> [1, 28, 29];
next(day,   "*", 29) -> [1, 29, 30];
next(day,   "*", 30) -> [1, 30, 31];
next(day,   "*", 31) -> [1, 31];
next(day,   "*",  D) -> [1, D, D+1];
next(day,   D,    _) -> [D];
next(month, "*", 12) -> [1, 12];
next(month, "*",  M) -> [1, M, M+1];
next(month, M,    _) -> [M];
next(year,  "*",  Y) -> [Y, Y+1, Y+2, Y+3, Y+4];
next(year,  Y,    _) -> [Y].

fetch(min,   [_,_,_,_,M]) -> M;
fetch(hour,  [_,_,_,H,_]) -> H;
fetch(day,   [_,_,D,_,_]) -> D;
fetch(month, [_,M,_,_,_]) -> M;
fetch(year,  [Y,_,_,_,_]) -> Y.

is_spec([Y,M,D,Hr,Min]) -> true;
is_spec(_)              -> false.

is_mfa({M,F,A})
  when erlang:is_atom(M),
       erlang:is_atom(F),
       erlang:is_list(A) -> true;
is_mfa(_)                -> false.


%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
next_test() ->
  [2010,2,2,0,0] = next_run([2010,2,"*","*","*"], [2010,2,1,59,59]),
  ok.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
