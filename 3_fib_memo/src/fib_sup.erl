
-module(fib_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, % restart only the crashed worker
           5, 10 % restart 10 times max within 5 sec
          }, 
          [{fib_serv,
            {fib_serv, start_link, []}, % M,F,A 
            permanent, % always restarted
            1000,      % 1000 millisec to shutdown orderly
            worker, []}
          ]}}.
