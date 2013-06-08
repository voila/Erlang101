-module(fib).

%% Application callbacks
-export([start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(fib).

