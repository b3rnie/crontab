%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc parse spec, calculate next runtime
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
katrihyvarinen@hotmail.com
%%%_* Module declaration ===============================================
-module(scheduler_time).
-compile(export_all).
-compile(no_auto_import, [now/0]).

%%%_* Exports ==========================================================
-export([ validate_spec/1
        , find_next/2
        , now/0
        , max/1
        ]).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
%%%_ * API -------------------------------------------------------------
validate_spec([_,_,_,_,_] = Spec) ->
  first_fail(fun do_validate/1, lists:zip(units(), Spec));
validate_spec(_Spec) ->
  {error, spec_format}.

find_next(Spec, Starttime) ->
  find_next(Spec, StarTime, units(), []).

now() ->
  {{Year, Month, Day}, {Hour, Minute, _Second}} = calendar:local_time(),
  [Year, Month, Day, Hour, Minute].

max(L) ->
  lists:max(L).

%%%_ * Internal spec validation ----------------------------------------
do_validate({_Unit, "*"}) -> ok;
do_validate({year, Year}) ->
  case is_integer_range(Year, 0, inf) of
    true  -> ok;
    false -> {error, spec_year}
  end;
do_validate({month, Month}) ->
  case is_integer_range(Month, 1, 12) of
    true  -> ok;
    false -> {error, spec_month}
  end;
do_validate({day, monday})    -> ok;
do_validate({day, tuesday})   -> ok;
do_validate({day, wednesday}) -> ok;
do_validate({day, thursday})  -> ok;
do_validate({day, friday})    -> ok;
do_validate({day, saturday})  -> ok;
do_validate({day, sunday})    -> ok;
do_validate({day, Day}) ->
  case is_integer_range(Day, 1, 31) of
    true  -> ok;
    false -> {error, spec_day}
  end;
do_validate({hour, Hr}) ->
  case is_integer_range(Hr, 0, 23) of
    true  -> ok;
    false -> {error, spec_hour}
  end;
do_validate({minute, Min}) ->
  case is_integer_range(Min, 0, 59) of
    true  -> ok;
    false -> {error, spec_minute}
  end.


is_integer_range(N,  _Start, _End) when not erlang:is_integer(N) -> false;
is_integer_range(_N, inf,    inf)                                -> true;
is_integer_range(N,  inf,    End) when N =< End                  -> true;
is_integer_range(N,  Start,  inf)  when N >= Start               -> true;
is_integer_range(N,  Start,  End) -> N >= Start andalso N =< End.

first_fail(F, [H|T]) ->
  case F(H) of
    ok           -> first_fail(F, T);
    {error, Rsn} -> {error, Rsn}
  end;
first_fail(_F, []) -> ok.

%%%_ * Internal next calculation ---------------------------------------
%% + 1 least significant until a match is found
find_next(Spec, Starttime, [Unit|Units], Acc) ->
  Next = next(Unit, Spec, Starttime),
  find_next(Spec, Starttime, Units, Next, Acc).
find_next(Spec, Starttime, Units, Next, Acc) ->
  find_next(Spec, Starttime, Units, Next, [N|Acc]);



is_valid_match(Time, Starttime) when Time =< Starttime -> false;
is_valid_match(Time, Starttime) ->
  calendar:valid_date(Y, M, D),
  calendar:day_of_week(Y, M, D),
  

next_time(Spec, StartTime, [Unit|Units], Res) ->
  fetch(Unit, Spec),
  fetch(Unit, StartTime),
  

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

next(year,  "*", Y ) -> [Y, Y+1, Y+2, Y+3, Y+4];
next(year,  Y,   _ ) -> [Y];
next(month, "*", 12) -> [12, 1];
next(month, "*", M ) -> [M, M+1, 1];
next(month, M,   _ ) -> [M];
next(day,   "*", 28) -> [1, 28, 29];
next(day,   "*", 29) -> [1, 29, 30];
next(day,   "*", 30) -> [1, 30, 31];
next(day,   "*", 31) -> [1, 31];
next(day,   "*",  D) -> [1, D, D+1];
next(day,   S,    D) -> next_day(S, D);
next(hour,  "*", 23) -> [0, 23];
next(hour,  "*",  H) -> [0, H, H+1];
next(hour,  H,    _) -> [H];
next(minute, "*", 59) -> [0];
next(minute, "*",  M) -> [0, M+1];
next(minute, M,    _) -> [M];

next_day(Spec, Day) when is_integer(Spec) -> [Day];
next_day(_Spec, Day) -> lists:seq(1, 31). %% ugly hack for now

fetch(year,   [Y,_,_,_,_]) -> Y.
fetch(month,  [_,M,_,_,_]) -> M;
fetch(day,    [_,_,D,_,_]) -> D;
fetch(hour,   [_,_,_,H,_]) -> H;
fetch(minute, [_,_,_,_,M]) -> M;

units() -> [year, month, day, hour, minute].

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

integer_range_test() ->
  true = is_integer_range(0, -1, 1),
  true = is_integer_range(0, inf, inf),
  true = is_integer_range(0, inf, 1),
  true = is_integer_range(1, -1, inf),
  false = is_integer_range(5, 0, 4),
  false = is_integer_range(5, 6, 9),
  

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
