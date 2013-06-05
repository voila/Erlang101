-module(birthday_dia).

-export([check_birthdays/0]).

% defines a person record, with 3 fields
-record(person,{name, 
                gender, 
                dob      %% date of birth: a {Year, Month, Day} tuple
               }). 



%%----------------------------------------------------
%% There is a bug in check_birthdays/0, instead of
%% Date is bound to the atom 'today' instead of today's date.
%% The code will compile, but when we call birthday_dia:check_birthdays(),
%% Lisa's birthday will always be missed.
%% 
%% If we run dialyzer (a static analysis tool), like this:
%% $ dialyzer birthday_dia.erl
%% it will warn you that you are calling is_birthday/2 with an
%% atom instead of a date as its first arguments
%%
%% Before you can run dializer, you need to initialize its table
%% like this:
%% $ dialyzer --build_plt --apps kernel stdlib
%% (warning: it takes a while)
%% See: http://www.erlang.org/doc/man/dialyzer.html
%%----------------------------------------------------


%% a list of persons
people() -> 
    Today = today(),
    [#person{name="joe", gender=m, dob={1978,3,17}},
     #person{name="lisa", gender=f, dob=Today}, %% it's always Lisa's birthday !
     #person{name="sammy", gender=m, dob={1988,6,4}}].


%% type definitions used by Dialyzer
-type date() :: {pos_integer(), pos_integer()}.
-spec is_birthday(date(), #person{}) -> [atom()].

%% given a date and a person, test for their birthday
is_birthday({_, Mon, Day}, #person{dob={_, Mon, Day}}) -> true; 
is_birthday(_,_) -> false.

%% check people's birthday
check_birthdays() ->
    Date = today,  %% mistake here, should be today()
    Greet = fun(P) -> io:format("Happy Birthday ~p~n",[P#person.name]) end,
    [Greet(P) || P <- people(), is_birthday(Date,P)].



%% today's date as a {Year, Month, Day} tuple
today() ->
    {Date,_} = calendar:local_time(), 
    Date.
