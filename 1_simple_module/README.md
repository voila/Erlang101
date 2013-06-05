### Check birthdays

This is a simple module to show pattern-matching and iteration

Given a list of persons (as records holding their date of birth), 
we want to check if today is anybody's birthday.

* check_birthdays_1/0 uses a hand-rolled loop
* check_birthdays_2/0 uses list:foreach
* check_birthdays_3/0 uses a list comprehension

Within the folder, start Erlang:

    $ erl
    
Load the module from the shell:

    > c(birthday).
    
Call the functions check_birthdays_{1,2,3}:

    > birthday:check_birthdays_1().

### Dialyzer

Dialyzer is a static analysis tool that can leverage type annotation in your code

Before you can run dializer the first time, you need to initialize its table like this:

    $ dialyzer --build_plt --apps kernel stdlib

Warning: it does take a while. See [Dialyzer's doc](http://www.erlang.org/doc/man/dialyzer.html).

There is a bug in the module **birthday_dia** (a typo I really did make when writing the code!).

In check_birthdays/0, Date is bound to the atom 'today' instead of today's date.
The code will compile, but when we call birthday_dia:check_birthdays(),
Lisa's birthday will always be missed.

If we run dialyzer, like this:

    $ dialyzer birthday_dia.erl

it will warn you that you are calling is_birthday/2 with an atom instead of a date, as its first arguments.

See [Erlang's type specifications](http://www.erlang.org/doc/reference_manual/typespec.html).
