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
              , mfa
              , next
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
  {ok, TRef}  = timer:send_interval(?tick, tick),
  {ok, #s{tref = TRef}}.

handle_call({add, Name, Spec, MFA}, _From, #s{tasks = Tasks0} = S) ->
  case try_add({Name, Spec, MFA}, Tasks0) of
    {ok, Tasks}  -> {reply, ok,           S#s{tasks = Tasks}};
    {error, Rsn} -> {reply, {error, Rsn}, S}
  end;

handle_call({del, Name}, _From, #s{tasks = Tasks0} = S) ->
  case try_del(Name, Tasks0) of
    {ok, Tasks}  -> {reply, ok,           S#s{tasks = Tasks}};
    {error, Rsn} -> {reply, {error, Rsn}, S}
  end.

handle_cast(stop, S) ->
    {stop, normal, S}.

handle_info(tick, #s{tasks = Tasks} = S) ->
  {noreply, S#s{tasks = tick(Tasks)}};

handle_info({'EXIT', Pid, Rsn}, #s{tasks = Tasks0} = S) ->
  case lists:keytake(Pid, #task.pid, Tasks0) of
    {value, #task{name = Name, pid = Pid} = Task, Tasks} ->
      error_logger:info_msg("~p ~p stopped with reason: ~p",
                            [?MODULE, Name, Rsn]),
      {noreply, S#s{tasks = [Task#task{pid = undefined} | Tasks]}};
    false ->
      error_logger:info_msg("~p exit message: ~p",
                            [?MODULE, Rsn]),
      {noreply, S}
  end;

handle_info(Info, S) ->
  error_logger:info_msg("~p info msg: ~p", [?MODULE, Info]),
  {noreply, S}.

terminate(_Rsn, #s{tref = TRef}) ->
  _ = timer:cancel(TRef),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
try_add({Name, Spec, MFA}, Tasks) ->
  case lists:keymember(Name, #task.name, Tasks) of
    true  -> {error, task_exists};
    false ->
      case scheduler_time:find_next(Spec, scheduler_time:now()) of
        {ok, Time}   -> {ok, [#task{ name = Name
                                   , spec = Spec
                                   , mfa  = MFA
                                   , next = Time
                                   } | Tasks]};
        {error, Rsn} -> {error, Rsn}
      end
  end.

try_del(Name, Tasks0) ->
  case lists:keytake(Name, #task.name, Tasks0) of
    {value, #task{pid = undefined}, Tasks} -> {ok, Tasks};
    {value, #task{pid = Pid},       Tasks} -> exit(Pid, stopped),
                                              {ok, Tasks};
    false                                  -> {error, no_such_task}
  end.

tick(Tasks) ->
  Now = scheduler_time:now(),
  lists:foldl(fun(#task{next = Time} = Task, Acc) ->
                  case scheduler_time:max([Now, Time]) of
                    Now  -> case start_task(Task) of
                              {ok, NewTask} -> [NewTask | Acc];
                              {error, _Rsn} -> Acc
                            end;
                    Time -> [Task|Acc]
                  end
              end, [], Tasks).

start_task(#task{ spec = Spec
                , mfa  = {M, F, A}
                , next = Next
                , pid = undefined} = Task) ->
  Pid = erlang:spawn_link(M, F, A),
  case scheduler_time:find_next(Spec, Next) of
    {ok, Time}   -> {ok, Task#task{pid = Pid, next = Time}};
    {error, Rsn} -> error_logger:info_msg("~p no next found: ~p~n",
                                          [?MODULE, Rsn]),
                    {error, Rsn}
  end;
start_task(#task{ spec = Spec
                , next = Next
                , name = Name} = Task) ->
  error_logger:info_msg("~p not starting, overlapping: ~p~n",
                        [?MODULE, Name]),
  case scheduler_time:find_next(Spec, Next) of
    {ok, Time}   -> {ok, Task#task{next = Time}};
    {error, Rsn} -> error_logger:info_msg("~p no next found: ~p~n",
                                          [?MODULE, Rsn]),
                    {error, Rsn}
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
