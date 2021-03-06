%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
  {N, Req2} = cowboy_req:qs_val(<<"n">>, Req),
  Res = fib_serv:fibonacci(binary_to_integer(N)),
	{ok, Req3} = cowboy_req:reply(200, [], integer_to_binary(Res), Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.
