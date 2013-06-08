### Fib Memo

This intend to show how to use the State parameter of a gen_server

#### Building the application

We'll use [rebar](https://github.com/basho/rebar).

From within *3_fib_memo* folder:

    $ wget https://github.com/rebar/rebar/wiki/rebar
    $ chmod +x rebar 
    $ ./rebar compile

#### The application

The server calculates Fibonacci numbers and store results in a [ets table](http://www.erlang.org/doc/man/ets.html).
To do this, we've added modified the following functions in fib_serv.erl:

* init/1: initialize the server state to a new ets table
* handle_call({fib, N}, _From, _State): store new result in the ets table
* terminate/2: discard the ets table

#### 

Start Erlang, add ebin/ to the code path, and call fib:start/0

    $ erl -pa ebin -s fib
    
In the Erlang shell, make the following calls in succession 
(so you can see the server caching earlier results):

    > fib_serv:fibonacci(40).
    > fib_serv:fibonacci(40).

