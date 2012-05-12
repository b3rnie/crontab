%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(scheduler_server).
-behaviour(gen_server).

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
-record(s, { tasks = []
           , tref  = undefined
           }).

-record(task, { name :: atom()
              , spec
              , mfa  :: {_,_,_}
              , next :: [_Y,_M,_D,_Hr,_Min]
              , pid  :: pid() | undefined
              }).

%%%_ * API -------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop()               -> gen_server:cast(?MODULE, stop).
add(Name, Spec, MFA) -> gen_server:call({add, Name, Spec, MFA}).
del(Name)            -> gen_server:call({del, Name}).

%%%_ * gen_server callbacks --------------------------------------------
init(_Args) ->
  erlang:process_flag(trap_exit, true),
  {ok, TRef} = timer:send_interval(?tick, tick),
  {ok, #s{tref = TRef}}.

handle_call({add, Name, Spec, MFA}, _From, #s{tasks = Tasks0} = S) ->
  case try_add({Name, Spec, MFA}, Tasks0) of
    {ok, Tasks}  -> {reply, ok,           S#s{tasks = Tasks}};
    {error, Rsn} -> {reply, {error, Rsn}, S}
  end;

handle_call({del, Name}, _From, #s{tasks = Tasks0} = S) ->
  case try_del(Name, Tasks) of
    {ok, Tasks}  -> {reply, ok,           S#s{tasks = Tasks}};
    {error, Rsn} -> {reply, {error, Rsn}, S}
  end.

handle_cast(stop, S) ->
    {stop, normal, S}.

handle_info(tick, #s{tasks = Tasks} = S) ->
  {noreply, S#s{tasks = tick(Tasks)}};

handle_info({'EXIT', Pid, _Rsn}, #s{jobs = Jobs0} = S) ->
  error_logger:info_msg("~p: ~p", [?MODULE, Rsn]),
  case lists:keytake(Pid, #job.pid, Jobs0) of
    {value, #job{pid = Pid} = Job, Jobs} ->
      {noreply, S#s{jobs = [Job#job{pid = undefined} | Jobs]}};
    false ->
      {noreply, S}
  end;

handle_info(Info, S) ->
  error_logger:info_msg("~p: ~p", [?MODULE, Info]),
  {noreply, S}.

terminate(_Rsn, #s{tref = TRef}) ->
  timer:cancel(TRef),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
try_add({Name, Spec, MFA}, Tasks) ->
  case lists:keymember(Name, #t.name, Tasks) of
    true  -> {error, task_exists};
    false ->
      case scheduler_spec:next_run(Spec, scheduler_time:now()) of
        {ok, Time} ->
          Task = #task{ name = Name
                      , spec = Spec
                      , mfa  = MFA
                      , next = Time
                      },
          {ok, [Task|Tasks]};
        {error, Rsn} -> {error, Rsn}
      end
  end.

try_del(Name, Tasks0) ->
  case lists:keytake(Name, #t.name, Tasks0) of
    {value, #t{pid = undefined}, Tasks} -> {ok, Tasks};
    {value, #t{pid = Pid},       Tasks} -> exit(Pid, stopped),
                                           {ok, Tasks};
    false                               -> {ok, Tasks0}
  end.

tick(Tasks) ->
  Now = scheduler_time:now(),
  F = fun(#task{next = Time} = Task) when Time >= Now -> try_start(Task);
         (#task{}            = Task)                  -> Task
      end,
  lists:map(F, Tasks).

try_start(#task{ spec = Spec
               , mfa  = {M, F, A}
               , next = Next
               , pid = undefined} = Task) ->
  Pid = proc_lib:spawn_link(M, F, A),
  case scheduler_spec:next_run(Spec, Next) of
    {ok, Time}   -> Task#task{pid = Pid, next = Time};
    {error, Rsn} -> Task#task{pid = Pid, next = undefined}
  end;
try_start(#task{ spec = Spec
               , next = Next} = Task) ->
  case scheduler_spec:next_run(Spec, Next) of
    {ok, Time}   -> Task#task{next = Next};
    {error, Rsn} -> Task#task{next = undefined}
  end.

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
