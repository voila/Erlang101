-module(birthday).

-export([check_birthdays_1/0,       
         check_birthdays_2/0, 
         check_birthdays_3/0]). 

% defines a person record, with 3 fields
-record(person,{name, 
                gender, 
                dob      %% date of birth: a {Year, Month, Day} tuple
               }). 



%%----------------------------------------------------
%% given a list of persons, 
%% we want to check if today is anybody's birthday
%%----------------------------------------------------


%% a list of persons
people() -> 
    Today = today(),
    [#person{name="joe", gender=m, dob={1978,3,17}},
     #person{name="lisa", gender=f, dob=Today}, %% it's always Lisa's birthday !
     #person{name="sammy", gender=m, dob={1988,6,4}}].



%% given a date and a person, test for their birthday
is_birthday({_, Mon, Day}, #person{dob={_, Mon, Day}}) -> true; 
is_birthday(_,_) -> false.


%% check people's birthday (version 1)
%% we loop through the people list 
%% and apply is_birthday/2 to each person
check_birthdays_1() ->
    Date = today(),                %% take off (), it's an atom [dialyzer]
    loop(Date, people()).

%-type date() :: {pos_integer(), pos_integer()}.
%-type people() :: [#person{}].
%-spec loop(date(),people()) -> ok.
%% $ dialyzer syn.erl
loop(_Date, []) ->
    ok;
loop(Date, [P|Ps]) ->
    case is_birthday(Date,P) of
        true  -> io:format("Happy Birthday ~p~n",[P#person.name]);
        _Else -> ok
    end,
    loop(Date, Ps).



%% check people's birthday (version 2)
%% we use lists:foreach/2 instead of our hand-rolled loop
check_birthdays_2() ->
    Date = today(),
    lists:foreach(
      fun(P) -> 
              case is_birthday(Date,P) of
                  true  -> io:format("Happy Birthday ~p~n",[P#person.name]);
                  _Else -> ok
              end  
      end, people()).



%% check people's birthday (version 3)
%% we use a list comprehension
check_birthdays_3() ->
    Date = today(),
    Greet = fun(P) -> io:format("Happy Birthday ~p~n",[P#person.name]) end,
    [Greet(P) || P <- people(), is_birthday(Date,P)].



%% today's date as a {Year, Month, Day} tuple
today() ->
    {Date,_} = calendar:local_time(), 
    Date.
