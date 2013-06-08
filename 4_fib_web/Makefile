REBAR = ./rebar
PROJECT = fib

all: deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

start:
	exec erl -pa ebin -pa deps/*/ebin -s $(PROJECT) 

