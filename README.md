wcard - retrieve metadata from a website and print it
=====================================================

Accepts the website url through the 'stdin',
then prints it back with the title and description.

The title and description lines are prefixed with 'title: ' and 'desc: ',
they may span multiple lines,
but otherwise they are attempted to be printed 'as it', not decorated.

Functions as an example of loose parsing for Haskell Rady -module.

BUGS
----

Sometimes may fail on some websites and prints an error.
In this case you need to be a little insane:
Repeating the request may help.
