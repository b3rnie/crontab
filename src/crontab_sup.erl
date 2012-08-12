%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(crontab_sup).
-behaviour(supervisor).

%%%_* Exports ==========================================================
-export([ start_link/1
        , init/1
        ]).

%%%_* Code =============================================================
start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
  %% with any other restart strategy than this we need to persist
  %% state somewhere.
  RestartStrategy = {one_for_all, 0, 1},
  Kids = [ {crontab_server, {crontab_server, start_link, [Args]},
            permanent, 5000, worker, [crontab_server]}
         ],
  {ok, {RestartStrategy, Kids}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
