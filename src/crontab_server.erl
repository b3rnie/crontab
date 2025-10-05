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
        , add/4
	    , remove/2
        ]).

%% gen_server
-export([ init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3
        ]).

-ignore_xref([start_link/1]).

%%%_* Includes =========================================================
-include_lib("kernel/include/logger.hrl").

%%%_* Macros ===========================================================
-define(tick, 1000).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s, { tasks = gb_trees:empty() %% name -> task
           , queue = gb_trees:empty() %% time -> name
           , p2n   = dict:new()
	       , n2p   = dict:new()
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

add(Name, Spec, MFA, Options) ->
  gen_server:call(?MODULE, {add, {Name, Spec, MFA, Options}}).

remove(Name, Options) ->
  gen_server:call(?MODULE, {remove, {Name, Options}}).

%%%_ * gen_server callbacks --------------------------------------------
init([]) ->
  erlang:process_flag(trap_exit, true),
  {ok, TRef} = timer:send_interval(?tick, tick),
  {ok, #s{tref=TRef}}.

terminate(Rsn, S) ->
  {ok, cancel} = timer:cancel(S#s.tref),
  lists:foreach(fun({Pid,_Name}) ->
		    exit(Pid, Rsn)
		end, dict:to_list(S#s.p2n)).

handle_call({add, {Name, Spec, MFA, Options}}, _From, S) ->
  case do_add(Name, Spec, MFA, Options, S#s.tasks, S#s.queue, S#s.n2p) of
    {ok, {Tasks, Queue}} ->
      {reply, ok, S#s{tasks=Tasks, queue=Queue}};
    {error, Rsn} ->
      {reply, {error, Rsn}, S}
  end;
handle_call({remove, {Name, Options}}, _From, S) ->
  case do_remove(
         Name, Options, S#s.tasks, S#s.queue, S#s.p2n, S#s.n2p) of
    {ok, {Tasks, Queue, P2N, N2P}} ->
      {reply, ok, S#s{tasks=Tasks, queue=Queue, p2n=P2N, n2p=N2P}};
    {error, Rsn} ->
      {reply, {error, Rsn}, S}
  end.

handle_cast(Msg, S) ->
    {stop, {bad_cast, Msg}, S}.

handle_info(tick, S) ->
  {Tasks, Queue, P2N, N2P} =
    do_tick(S#s.tasks, S#s.queue, S#s.p2n, S#s.n2p),
  {noreply, S#s{ tasks = Tasks
	       , queue = Queue
	       , p2n   = P2N
	       , n2p   = N2P
	       }};
handle_info({'EXIT', Pid, Rsn}, S) ->
  Name = dict:fetch(Pid, S#s.p2n),
  case Rsn of
    normal -> ?LOG_INFO("~p done", [Name]);
    _      -> ?LOG_CRITICAL("~p crashed: ~p", [Name, Rsn])
  end,
  {noreply, S#s{ p2n = dict:erase(Pid, S#s.p2n)
	           , n2p = dict:erase(Name, S#s.n2p)
	           }};
handle_info(Msg, S) ->
  ?LOG_WARNING("~p", [Msg]),
  {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
do_add(Name, Spec, MFA, Options, Tasks, Queue, N2P) ->
  case gb_trees:is_defined(Name, Tasks) orelse dict:is_key(Name, N2P) of
    true  -> {error, task_exists};
    false ->
      case crontab_time:find_next(Spec, crontab_time:now()) of
	{ok, Time} ->
	  Task = #task{spec=Spec, mfa=MFA, next=Time, options=Options},
	  {ok, { gb_trees:insert(Name, Task, Tasks)
	       , gb_trees:insert({Time, Name}, Name, Queue)
	       }};
	{error, Rsn} ->
	  {error, Rsn}
      end
  end.

do_remove(Name, Options, Tasks, Queue, P2N0, N2P0) ->
  case gb_trees:lookup(Name, Tasks) of
    {value, #task{next=Time, options=TaskOptions}} ->
      {P2N, N2P} = maybe_stop(Name, Options ++ TaskOptions, P2N0, N2P0),
      {ok, { gb_trees:delete(Name, Tasks)
	   , gb_trees:delete_any({Time, Name}, Queue)
	   , P2N
           , N2P
           }};
    none ->
      {error, no_such_task}
  end.

maybe_stop(Name, Options, P2N, N2P) ->
  case proplists:get_value(stop_on_remove, Options, true) of
    true  ->
      case dict:find(Name, N2P) of
        {ok, Pid} ->
          %% TODO: add a testcase for this case
          ?LOG_INFO("~p stopping", [Name]),
          exit(Pid, removing),
          {P2N, N2P};
        error ->
          {P2N, N2P}
      end;
    false ->
      {P2N, N2P}
  end.

do_tick(Tasks0, Queue0, P2N0, N2P0) ->
  case gb_trees:size(Queue0) of
    0 -> {Tasks0, Queue0, P2N0, N2P0};
    _ -> Now = crontab_time:now(),
         case gb_trees:take_smallest(Queue0) of
           {{Time, Name}, Name, Queue1}
             when Time =< Now ->
             Task           = gb_trees:get(Name, Tasks0),
             {P2N, N2P}     = try_start(Name, Task, P2N0, N2P0),
             {Tasks, Queue} = try_schedule(Name, Task, Tasks0, Queue1),
             do_tick(Tasks, Queue, P2N, N2P);
           {{_Time, Name0}, Name1, _Queue} when Name0 =:= Name1 ->
             {Tasks0, Queue0, P2N0, N2P0}
         end
  end.

try_start(Name, Task, P2N, N2P) ->
  %% TODO: figure out what to do with overlapping tasks
  case dict:is_key(Name, N2P) of
    true ->
      ?LOG_WARNING("~p is running, not starting", [Name]),
      {P2N, N2P};
    false ->
      ?LOG_INFO("~p starting", [Name]),
      {M,F,A} = Task#task.mfa,
      Pid = erlang:spawn_link(M, F, A),
      {dict:store(Pid, Name, P2N),
       dict:store(Name, Pid, N2P)}
  end.

try_schedule(Name, Task, Tasks0, Queue0) ->
  case crontab_time:find_next(Task#task.spec, Task#task.next) of
    {ok, Time} ->
      ?LOG_INFO("~p next start: ~p", [Name, Time]),
      {gb_trees:update(Name, Task#task{next=Time}, Tasks0),
       gb_trees:insert({Time, Name}, Name, Queue0)};
    {error, Rsn} ->
      ?LOG_INFO("~p unable to find next start: ~p", [Name, Rsn]),
      {gb_trees:update(Name, Task#task{next=undefined}, Tasks0), Queue0}
  end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

empty_tick_test_() ->
  F = fun() -> timer:sleep(5000) end,
  {timeout, 10, fun() -> crontab_test:with_crontab(F) end}.

unable_to_schedule_test() ->
  crontab_test:with_crontab(
    fun() ->
	Spec = crontab_time:now(),
	{error, no_next_found} =
	  crontab:add(foo, Spec, {crontab_test, execute_funs,
				  [[fun() -> exit(fail) end]]})
    end).

stop_running_task_test_() ->
  {timeout, 120,
   fun() ->
       crontab_test:with_crontab(
         fun() ->
             Daddy = self(),
             Ref   = make_ref(),
             Spec  = ['*', '*', '*', '*', '*'],
             Fs    = [fun() ->
                          Daddy ! {Ref, self()},
                          timer:sleep(5000)
                      end],
             MFA   = {crontab_test, execute_funs, [Fs]},
             ok    = crontab:add(foo, Spec, MFA),
             Pid2  = receive {Ref, Pid} -> Pid end,
             Ref2  = erlang:monitor(process, Pid2),
             ok    = crontab:remove(foo, [{stop_on_remove, true}]),
             receive {'DOWN', Ref2, process, Pid2, Rsn} ->
                 ?assert(Rsn =:= removing)
             end,
             timer:sleep(1000),
             ok = crontab:add(foo, Spec, MFA)
         end)
   end}.

cover_test() ->
  crontab_test:with_crontab(
    fun() ->
        crontab_server ! foo,
        ok = sys:suspend(crontab_server),
        ok = sys:change_code(crontab_server, crontab_server, 0, []),
        ok = sys:resume(crontab_server)
    end).

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
