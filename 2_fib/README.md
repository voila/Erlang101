### Fib

This intend to show a minimal OTP application with a gen_server

#### Building the application

We'll use [rebar](https://github.com/basho/rebar).

From with the 2_fib folder:

    $ wget https://github.com/rebar/rebar/wiki/rebar
    $ chmod +x rebar 
    $ ./rebar compile

#### The application

* The OTP application is started in module **fib** with: application:start(fib).
* The name of the application 'fib' is defined in the file src/fib.app.src
* This file also defines the application module as **fib_app** 
* The module **fib_app** starts the top supervisor: fib_sup:start_link().
* The module **fib_sup** defines the top supervisor, It supervises only one process fib_serv.
* The module **fib_serv** implements the callbacks for our server


The server calculates Fibonacci numbers. In order to do this, we add the following code to fib_serv.erl:

* fibonacci/1: this is the server external API, this is how clients query the server
* handle_call({fib, N}, _From, _State): this handles the message {fib, N}
* fib/1: this is our server logic, it calculates a fibonacci number

#### Querying the server

Start Erlang, add ebin/ to the code path, and call fib:start/0

    $ erl -pa ebin -s fib
    
In the Erlang shell, query the server:

    > fib_serv:fibonacci(30).
    
#### Timeout

By default, handle_call will timeout after 5 seconds.

Use gen_server:call/3 to specify an infinite timeout.

You can reload the module from the shell, like this:

    > c("src/fib_serv.erl",[{output_dir, ebin}]).

#### Guard

The fib() function accepts negative integers and goes into an infinite loop.

Fix this with a [guard](http://www.erlang.org/doc/getting_started/seq_prog.html#id63162) and reload the module.

Make the following call: 

    > fib_serv:fibonacci(-10).

What happens? 

Hint: when no function clause applies to the input a runtime error occurs. Use i()., in the shell, to observe the running processes before and after the call.


#### Asynchronous query

The fib_server:fibonacci call blocks the server until the result is returned.

Because this call can take a lot of time to complete, it would be better to:

* spawn fib/1 in a new process
* return a response straight away to the client, to free the server loop
* send a message the client once we have the result

Look at fibonacci_async/1, handle_call({fib_async, N}, From, _State), fib_async/2

Make the following calls in succession (so you can see the server response to both):

    > fib_serv:fibonacci_async(40).
    > fib_serv:fibonacci_async(10).

You need to use flush(). in the shell, to see the messages received. 


