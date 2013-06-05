-module(fib).

%% Application callbacks
-export([start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(fib).

stop(_State) ->
    ok.
