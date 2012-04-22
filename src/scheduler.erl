%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc simple crontab hack
%%% @copyright Bjorn Jensen-Urstad 2011
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(scheduler).
-behaviour(gen_server).
-compile(export_all).

%%%_* Exports ==========================================================
-export([ start_link/1
        , stop/0
        , add/3
        , del/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-ignore_xref([ start_link/1
             , stop/0
             ]).

%%%_* Includes =========================================================
%%%_* Macros ===========================================================
-define(tick, 1000).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s, { jobs   :: list()
           , tref   :: _
           }).

-record(job, { id   :: any()
             , mfa  :: {_,_,_}
             , next :: [_Y,_M,_D,_Hr,_Min]
             , pid  :: pid() | undefined
             }).

%%%_ * API -------------------------------------------------------------
start(Args) ->
  gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
  gen_server:cast(?MODULE, stop).

add(Id, Spec, MFA) ->
  gen_server:call({add, Id, Spec, MFA}).

del(Id) ->
  gen_server:call({del, Id}).

%%%_ * gen_server callbacks --------------------------------------------
init(_Args) ->
  erlang:process_flag(trap_exit, true),
  {ok, TRef} = timer:send_interval(?tick, tick),
  {ok, #s{tref = TRef}}.

handle_call({add, Id, Spec, MFA}, _From, S) ->
    {reply, ok, S};

handle_call({del, Id}, _From, S) ->
  case dict:is_key(Id, Jobs0) of
    true  -> Jobs = dict:erase(Id, Jobs0),
             {reply, ok, S#s{jobs = Jobs}};
    false -> {reply, {error, no_such_job}, S}
  end.

handle_cast(stop, S) ->
    {stop, normal, S}.

handle_info(tick, #s{} = S) ->
  %% calendar:datetime_to_gregorian_seconds(),
  {noreply, S};

handle_info({'EXIT', Pid, _Rsn}, S) ->
  {noreply, S};

handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Rsn, #s{tref = TRef}) ->
  timer:cancel(TRef),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
%% naive search for next runtime
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
