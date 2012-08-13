%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(crontab_server).
-behaviour(gen_server).

%%%_* Exports ==========================================================
-export([ start_link/1
        , stop/0
        , add/3
        , del/1
        ]).

-export([ init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3
        ]).

-ignore_xref([ start_link/1
             , stop/0
             ]).

%%%_* Includes =========================================================
-include_lib("crontab/include/crontab.hrl").

%%%_* Macros ===========================================================
-define(tick, 1000).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s, { tasks    = dict:new()       %% {k:name, v:#task{}}
           , queue    = gb_trees:empty() %% {k:{time, name}, v:name
           , running  = dict:new()       %% {k:pid, v:name} | {k:name, v:pid}
           , tref
           }).

-record(task, { spec
              , mfa
              , next
              , options
              }).

%%%_ * API -------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
  gen_server:cast(?MODULE, stop).

add(Name, Spec, MFA) ->
  add(Name, Spec, MFA, []).

add(Name, Spec, MFA, Options) ->
  gen_server:call(?MODULE, {add, Name, Spec, MFA, Options}).

del(Name) ->
  gen_server:call(?MODULE, {del, Name}).

%%%_ * gen_server callbacks --------------------------------------------
init(_Args) ->
  erlang:process_flag(trap_exit, true),
  {ok, TRef} = timer:send_interval(?tick, tick),
  {ok, #s{tref=TRef}}.

terminate(_Rsn, S) ->
  timer:cancel(S#s.tref),
  lists:foreach(fun({K,_V}) when erlang:is_pid(K)  -> ok;
                   ({K,_V}) when erlang:is_atom(K) ->
                    exit(K, stopped)
                end, dict:to_list(S#s.running)),
  ok.

handle_call({add, Name, Spec, MFA, Options}, _From, S) ->
  ?debug("adding task ~p", [Name]),
  case do_add({Name, Spec, MFA, Options}, S#s.tasks, S#s.queue) of
    {ok, {Tasks, Queue}} -> {reply, ok, S#s{tasks=Tasks, queue=Queue}};
    {error, Rsn}         -> {reply, {error, Rsn}, S}
  end;

handle_call({del, Name}, _From, S) ->
  ?debug("deleting task ~p", [Name]),
  case do_del(Name, S#s.tasks, S#s.queue) of
    {ok, {Tasks, Queue}} -> {reply, ok, S#s{tasks=Tasks, queue=Queue}};
    {error, Rsn}         -> {reply, {error, Rsn}, S}
  end.

handle_cast(stop, S) ->
    {stop, normal, S}.

handle_info(tick, S) ->
  {noreply, do_tick(S)};

handle_info({'EXIT', Pid, Rsn}, #s{running=Running0} = S) ->
  Name     = dict:fetch(Pid, Running0),
  Running1 = dict:erase(Pid, Running0),
  Running  = dict:erase(Name, Running1),
  ?warning("task ~p done: ~p", [Name, Rsn]),
  {noreply, S#s{running=Running}};

handle_info(Msg, S) ->
  ?warning("~p", [Msg]),
  {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
do_add({Name, Spec, Mfa, Options}, Tasks0, Queue0) ->
  case dict:is_key(Name, Tasks0) of
    true  -> {error, task_exists};
    false ->
      case crontab_time:find_next(Spec, crontab_time:now()) of
        {ok, Time} ->
          Task  = #task{spec=Spec, mfa=Mfa, next=Time, options=Options},
          Tasks = dict:store(Name, Task, Tasks0),
          Queue = gb_trees:insert({Time, Name}, Name, Queue0),
          {ok, {Tasks, Queue}};
        {error, Rsn} ->
          {error, Rsn}
      end
  end.

do_del(Name, Tasks0, Queue0) ->
  case dict:find(Name, Tasks0) of
    {ok, #task{next=undefined}} ->
      {ok, {dict:erase(Name, Tasks0), Queue0}};
    {ok, #task{next=Next}} ->
      Tasks = dict:erase(Name, Tasks0),
      Queue = gb_trees:delete({Next, Name}, Queue0),
      {ok, {Tasks, Queue}};
    error ->
      {error, no_such_task}
  end.

do_tick(S) ->
  case gb_trees:size(S#s.queue) of
    0 -> S;
    _ -> Now = crontab_time:now(),
         case gb_trees:take_smallest(S#s.queue) of
           {{Time, Name}, Name, Queue0}
             when Time =< Now ->
             Task           = dict:fetch(Name, S#s.tasks),
             Running        = try_start(Name, Task, S#s.running),
             {Tasks, Queue} = try_schedule(Name, Task, S#s.tasks, Queue0),
             do_tick(S#s{running=Running, queue=Queue, tasks=Tasks});
           {{_Time, _Name}, _Name, _Queue} ->
             S
         end
  end.

try_start(Name, Task, Running0) ->
  case dict:is_key(Name, Running0) of
    true ->
      ?warning("~p is still running, not starting", [Name]),
      Running0;
    false ->
      ?debug("starting ~p", [Name]),
      {M,F,A} = Task#task.mfa,
      Pid = erlang:spawn_link(M, F, A),
      Running1 = dict:store(Pid, Name, Running0),
      _Running = dict:store(Name, Pid, Running1)
  end.

try_schedule(Name, Task, Tasks0, Queue0) ->
  case crontab_time:find_next(Task#task.spec, Task#task.next) of
    {ok, Time} ->
      ?debug("scheduling ~p: ~p", [Name, Time]),
      Tasks = dict:store(Name, Task#task{next=Time}, Tasks0),
      Queue = gb_trees:insert({Time, Name}, Name, Queue0),
      {Tasks, Queue};
    {error, Rsn} ->
      ?debug("unable to schedule ~p: ~p", [Name, Rsn]),
      {dict:store(Name, Task#task{next=undefined}, Tasks0), Queue0}
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
