%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc validate spec, calculate next runtime
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(crontab_time).
-compile(export_all).
-compile({no_auto_import, [now/0]}).

%%%_* Exports ==========================================================
-export([ validate_spec/1
        , find_next/2
        , now/0
        , max/1
        ]).

%%%_* Includes =========================================================
-include_lib("crontab/include/crontab.hrl").

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
validate_spec([_,_,_,_,_] = Spec) ->
  try_all(fun do_validate/1, lists:zip(units(), Spec));
validate_spec(_Spec) ->
  {error, spec_format}.

find_next(Spec, From) ->
  [U|Us] = units(),
  Start  = next(U, fetch(U, Spec), fetch(U, From)),
  find_next(Spec, From, Us, [[E] || E <- Start]).

now() ->
  {{Y,M,D}, {Hr,Min,_Sec}} = calendar:local_time(),
  [Y, M, D, Hr, Min].

max(L) ->
  lists:max(L).

%%%_ * Internal spec validation ----------------------------------------
do_validate({_U, "*"}) -> ok;
do_validate({year, Y}) ->
  case in_range(Y, 0, inf) of
    true  -> ok;
    false -> {error, spec_year}
  end;
%% TODO: maybe support something like this aswell
%% do_validate({month, january}) -> ok;
%% do_validate({month, february}) -> ok;
do_validate({month, M}) ->
  case in_range(M, 1, 12) of
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
do_validate({day, D}) ->
  case in_range(D, 1, 31) of
    true  -> ok;
    false -> {error, spec_day}
  end;
do_validate({hour, H}) ->
  case in_range(H, 0, 23) of
    true  -> ok;
    false -> {error, spec_hour}
  end;
do_validate({minute, M}) ->
  case in_range(M, 0, 59) of
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
%% The way this works:
%% Step 1. Generate all possible dates looking at a spec and from date,
%%         what is possible is decided by the next/3 function.
%% Step 2. Filter out invalid dates and passed ones and pick the
%%         smallest one.
find_next(Spec, From, [U|Us], Dates) ->
  Nexts = next(U, fetch(U, Spec), fetch(U, From)),
  find_next(Spec, From, Us, [[Next|Date] || Date <- Dates,
                                            Next <- Nexts]);
find_next(Spec, From, [], Alldates) ->
  %% ?debug("alldates: ~p", [Alldates]),
  Fs = [ fun(Dates) -> reverse_order(Dates) end
       , fun(Dates) -> filter_invalid_ymd(Dates) end
       , fun(Dates) -> filter_invalid_day(Dates, Spec) end
       , fun(Dates) -> filter_passed(Dates, From) end
       ],
  case lists:sort(lists:foldl(fun(F, X) -> F(X) end, Alldates, Fs)) of
    [Next|_] -> {ok, Next};
    []       -> {error, no_next_found}
  end.

reverse_order(Dates) ->
  lists:map(fun(Date) -> lists:reverse(Date) end, Dates).

filter_invalid_ymd(Dates) ->
  lists:filter(fun(Date) ->
                   [Y,M,D] = fetch([year, month, day], Date),
                   calendar:valid_date(Y, M, D)
               end, Dates).

filter_invalid_day(Dates, Spec) ->
  Day = fetch(day, Spec),
  case erlang:is_atom(Day) of
    true  -> do_filter_invalid_day(Dates, Spec);
    false -> Dates
  end.

do_filter_invalid_day(Dates, Spec) ->
  Day = fetch(day, Spec),
  lists:filter(fun(Date) ->
                   [Y,M,D] = fetch([year, month, day], Date),
                   Day =:= day_of_the_week(Y, M, D)
               end, Dates).

filter_passed(Dates, From) ->
  lists:filter(fun(Date) -> Date > From end, Dates).

next(year,   "*", Y ) -> [Y, Y+1, Y+2, Y+3, Y+4];
next(year,   Y,   _ ) -> [Y];
next(month,  "*", 12) -> [12, 1];
next(month,  "*", M ) -> [M, M+1, 1];
next(month,  M,   _ ) -> [M];
next(day,    "*", 28) -> [1, 28, 29];
next(day,    "*", 29) -> [1, 29, 30];
next(day,    "*", 30) -> [1, 30, 31];
next(day,    "*", 31) -> [1, 31];
next(day,    "*", D ) -> [1, D, D+1];
next(day,    S,   _ )
  when is_atom(S)     -> lists:seq(1, 31);
next(day,    S,   _ ) -> [S];
next(hour,   "*", 23) -> [0, 23];
next(hour,   "*", H ) -> [0, H, H+1];
next(hour,   H,   _ ) -> [H];
next(minute, "*", 59) -> [0];
next(minute, "*", M ) -> [0, M+1];
next(minute, M,   _ ) -> [M].

fetch(year,   [Y,_,_,_,_]       ) -> Y;
fetch(month,  [_,M,_,_,_]       ) -> M;
fetch(day,    [_,_,D,_,_]       ) -> D;
fetch(hour,   [_,_,_,H,_]       ) -> H;
fetch(minute, [_,_,_,_,M]       ) -> M;
fetch(Us,     [_,_,_,_,_] = Spec) -> [fetch(U, Spec) || U <- Us].

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
%% -include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% TODO: needs to be extended to cover more complex searches
-define(skew, 10).

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

every_day_test() ->
  From = [2012, 1, 1, 0, 0],
  Spec = ["*", "*", "*", 0, 0],
  Stop = fun([Y,_,_,_,_]) -> Y =:= 2018 end,
  Cond = fun(GSecs1, GSecs2) ->
             Diff = GSecs2 - GSecs1,
             Diff > (86400-?skew) andalso Diff < (86400+?skew)
         end,
  {ok, Next} = find_next(Spec, From),
  iterate(Next, Stop, Spec, Cond).

every_week_test() ->
  From = [2012,1,1,0,0],
  Cond = fun(GSecs1, GSecs2) ->
             Diff = GSecs2 - GSecs1,
             Week = 86400 * 7,
             Diff > (Week-?skew) andalso Diff < (Week+?skew)
         end,
  Stop = fun([Y,_,_,_,_]) -> Y =:= 2014 end,
  lists:foreach(fun(DayInt) ->
                    Spec = ["*","*",day_of_the_week(DayInt),0,0],
                    ok = validate_spec(Spec),
                    {ok, Next} = find_next(Spec, From),
                    iterate(Next, Stop, Spec, Cond)
                end, lists:seq(1, 7)).

every_hour_test() ->
  From = [2012,10,1,0,0],
  Cond = fun(GSecs1, GSecs2) ->
             Diff = GSecs2 - GSecs1,
             Diff > (3600-?skew) andalso Diff < (3600+?skew)
         end,
  Stop = fun([_,M,_,_,_]) -> M =:= 3 end,
  Spec = ["*", "*", "*", "*", 0],
  {ok, Next} = find_next(Spec, From),
  iterate(Next, Stop, Spec, Cond).

iterate([Y1,M1,D1,H1,Min1] = Prev, Stop, Spec, Cond) ->
  {ok, [Y2,M2,D2,H2,Min2]=Next} = find_next(Spec, Prev),
  GSecs1 = calendar:datetime_to_gregorian_seconds({{Y1,M1,D1},{H1,Min1,0}}),
  GSecs2 = calendar:datetime_to_gregorian_seconds({{Y2,M2,D2},{H2,Min2,0}}),
  true = Cond(GSecs1, GSecs2),
  case Stop(Next) of
    true  -> ok;
    false -> iterate(Next, Stop, Spec, Cond)
  end.

next_test() ->
  lists:foreach(fun({Spec, Now, Expected}) ->
                    ok = validate_spec(Spec),
                    {ok, Expected} = find_next(Spec, Now)
                end, tests()),
  ok.

tests() ->
  %% {Spec, Now, Expected}
  [ {["*",  2, 29,     20,  0 ],  [2012, 3,  1,0,0],  [2016,2,29,20,0]}
  , {["*",  1, friday, 0,   0 ],  [2012, 1,  1,1,43], [2012,1,6,0,0]}
  , {[2014, 9, 15,     "*", 20],  [2013, 11, 28, 12, 3], [2014,9,15,0,20]}
  , {["*",   2, "*",    0,   0 ], [2012, 2,  28, 0,  0], [2012, 2, 29, 0, 0]}
  , {["*",  "*", 30,  "*",   "*"],[2012, 4,  30, 0, 59], [2012, 4, 30, 1, 0]}
  ].
-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
