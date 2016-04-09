Erlang code reloader. Based on Mochiweb's reloader.

The original version has a fixed poll interval of 1 second.

This version has the polling interval configurable.

rebar.conf

    {deps, [
        {reloader, ".*",
            {hg, "https://bitbucket.org/marco_m/reloader", "default"}}
    ]}.


Start Erlang runtime, tell reloader to poll:

If compiling via `rebar compile`:

    # Ask reloader to poll to reload the code each 2 seconds.
    $ erl -pa deps/*/ebin -pa ebin -run reloader start 2

If compiling via `rebar eunit`:

    # Ask reloader to poll to reload the code each 2 seconds.
    $ erl -pa deps/*/ebin -pa .eunit -run reloader start 2
