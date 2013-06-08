### Fib Web

We'll use [Cowboy](https://github.com/extend/cowboy) to add a web interface to our fibonacci application. Cowboy, a web server written in Erlang, is just another OTP application running alongside ours. 

#### Rebar

We'll use [rebar](https://github.com/basho/rebar).

To install it, from within *4_fib_web*:

    $ wget https://github.com/rebar/rebar/wiki/rebar
    $ chmod +x rebar 

#### The application

In src/fib.app.src, we add cowboy to the list of applications we're depending on:

    {applications, [
                    kernel,
                    stdlib,
                    cowboy
                 ]},

In src/fib.erl, we start cowboy and its dependencies, before starting our application: 

    start() ->
        ok = application:start(crypto),
        ok = application:start(ranch),
        ok = application:start(cowboy),
        ok = application:start(fib).


In src/fib_app.erl, we start Cowboy HTTP listener on port 8080,
and specify that route "/" be handled by *toppage_handler*, before starting our supervisor.

    start(_StartType, _StartArgs) ->
        Dispatch = cowboy_router:compile([
          {'_', [{"/", toppage_handler, []}]}
        ]),
        {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], 
                             [{env, [{dispatch, Dispatch}]}]),
        fib_sup:start_link().

Finally, in src/toppage_handler.erl, we implement the *handle* callback. We look up parameter "n", ask *fib_serv* for fibonacci of n, and build a reply with the result:

    handle(Req, State) ->
      {N, Req2} = cowboy_req:qs_val(<<"n">>, Req),
      Res = fib_serv:fibonacci(binary_to_integer(N)),
      {ok, Req3} = cowboy_req:reply(200, [], integer_to_binary(Res), Req2),
      {ok, Req3, State}.

#### Building the application and its dependencies

We use rebar and a simple Makefile. 

To fetch the dependencies and build the application, type:

    $ make 

#### Starting the application

To start Erlang, type

    $ make start
    
Open a browser at: [http://localhost:8080/?n=24](http://localhost:8080/?n=24) and try different values for n.

