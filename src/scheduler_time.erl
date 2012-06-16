%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc validate spec, calculate next runtime
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(scheduler_time).
-compile(export_all).
-compile({no_auto_import, [now/0]}).

%%%_* Exports ==========================================================
-export([ validate_spec/1
        , find_next/2
        , now/0
        , max/1
        ]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
validate_spec([_,_,_,_,_] = Spec) ->
  try_all(fun do_validate/1, lists:zip(units(), Spec));
validate_spec(_Spec) ->
  {error, spec_format}.

find_next(Spec, Starttime) ->
  [Unit|Units] = units(),
  Start        = next(Unit, fetch(Unit, Spec), fetch(Unit, Starttime)),
  find_next(Spec, Starttime, Units, lists:map(fun(E) -> [E] end, Start)).

now() ->
  {{Year, Month, Day}, {Hour, Minute, _Second}} = calendar:local_time(),
  [Year, Month, Day, Hour, Minute].

max(L) ->
  lists:max(L).

%%%_ * Internal spec validation ----------------------------------------
%% TODO: Support list of times / unit. ie hour [12,13,14].
do_validate({_Unit, "*"}) -> ok;
do_validate({year, Year}) ->
  case in_range(Year, 0, inf) of
    true  -> ok;
    false -> {error, spec_year}
  end;
do_validate({month, Month}) ->
  case in_range(Month, 1, 12) of
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
  case in_range(Day, 1, 31) of
    true  -> ok;
    false -> {error, spec_day}
  end;
do_validate({hour, Hr}) ->
  case in_range(Hr, 0, 23) of
    true  -> ok;
    false -> {error, spec_hour}
  end;
do_validate({minute, Min}) ->
  case in_range(Min, 0, 59) of
    true  -> ok;
    false -> {error, spec_minute}
  end.

in_range(N,  _S,  _E ) when not erlang:is_integer(N) -> false;
in_range(_N, inf, inf)                               -> true;
in_range(N,  inf, E  ) when N =< E                   -> true;
in_range(N,  S,   inf) when N >= S                   -> true;
in_range(N,  S,   E  ) when N>=S, N=<E               -> true;
in_range(_N, _S,  _E )                               -> false.

try_all(F, [H|T]) ->
  case F(H) of
    ok           -> try_all(F, T);
    {error, Rsn} -> {error, Rsn}
  end;
try_all(_F, []) -> ok.

%%%_ * Internal next calculation ---------------------------------------
find_next(Spec, Starttime, [Unit|Units], Candidates) ->
  Nexts = next(Unit, fetch(Unit, Spec), fetch(Unit, Starttime)),
  find_next(Spec, Starttime, Units,
            [Candidate ++ [Next] || Candidate <- Candidates,
                                    Next      <- Nexts]);
find_next(Spec, Starttime, [], Candidates0) ->
  io:format("Candidates0: ~p~n", [Candidates0]),
  Fs = [ fun(C) -> filter_invalid_ymd(C) end
       , fun(C) -> filter_invalid_day(C, Spec) end
       , fun(C) -> filter_passed(C, Starttime) end
       , fun(C) -> lists:sort(C) end
       ],
  case lists:foldl(fun(Fun, Acc) -> Fun(Acc) end,
                   Candidates0, Fs) of
    [Next|_] -> {ok, Next};
    []       -> {error, no_next_found}
  end.

filter_invalid_ymd(C) ->
  lists:filter(fun(E) ->
                   [Y, M, D] = fetch([year, month, day], E),
                   calendar:valid_date(Y, M, D)
               end, C).

filter_invalid_day(C, Spec) ->
  case fetch(day, Spec) of
    Day when is_atom(Day) ->
      lists:filter(fun(E) ->
                       [Y, M, D] = fetch([year, month, day], E),
                       Day =:= day_of_the_week(Y, M, D)
                   end, C);
    _Day -> C
  end.

filter_passed(C, Starttime) ->
  lists:filter(fun(Date) ->
                   Date > Starttime
               end, C).

next(year,   "*", Y )                 -> [Y, Y+1, Y+2, Y+3, Y+4];
next(year,   Y,   _ )                 -> [Y];
next(month,  "*", 12)                 -> [12, 1];
next(month,  "*", M )                 -> [M, M+1, 1];
next(month,  M,   _ )                 -> [M];
next(day,    "*", 28)                 -> [1, 28, 29];
next(day,    "*", 29)                 -> [1, 29, 30];
next(day,    "*", 30)                 -> [1, 30, 31];
next(day,    "*", 31)                 -> [1, 31];
next(day,    "*", D )                 -> [1, D, D+1];
next(day,    S,   _ ) when is_atom(S) -> lists:seq(1, 31); %% TODO
next(day,    S,   _ )                 -> [S];
next(hour,   "*", 23)                 -> [0, 23];
next(hour,   "*", H )                 -> [0, H, H+1];
next(hour,   H,   _ )                 -> [H];
next(minute, "*", 59)                 -> [0];
next(minute, "*", M )                 -> [0, M+1];
next(minute, M,   _ )                 -> [M].

fetch(year,   [Y,_,_,_,_]    ) -> Y;
fetch(month,  [_,M,_,_,_]    ) -> M;
fetch(day,    [_,_,D,_,_]    ) -> D;
fetch(hour,   [_,_,_,H,_]    ) -> H;
fetch(minute, [_,_,_,_,M]    ) -> M;
fetch(L,      [_,_,_,_,_] = S) -> [fetch(Unit, S) || Unit <- L].

units() -> [year, month, day, hour, minute].

day_of_the_week(Y, M, D) ->
  day_of_the_week(calendar:day_of_the_week(Y, M, D)).

day_of_the_week(1) -> monday;
day_of_the_week(2) -> tuesday;
day_of_the_week(3) -> wednesday;
day_of_the_week(4) -> thursday;
day_of_the_week(5) -> friday;
day_of_the_week(6) -> saturday;
day_of_the_week(7) -> sunday.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

in_range_test() ->
  true  = in_range(0, -1, 1),
  true  = in_range(0, inf, inf),
  true  = in_range(0, inf, 1),
  true  = in_range(1, -1, inf),
  false = in_range(5, 0, 4),
  false = in_range(5, 6, 9),
  false = in_range(x, inf, inf),
  false = in_range(x, 0, 10),
  ok.

next_test() ->
  lists:foreach(fun({Spec, Now, Expected}) ->
                    Expected = find_next(Spec, Now)
                end, tests()),
  ok.

tests() ->
  [{["*",2,29,20,0], [2012,3,1,0,0], [2016,2,29,20,0]}].

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
