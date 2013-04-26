# EHF - Erlang Helper Function Library

This is a collection of useful Erlang functions that I collected over
time. It can be used as snippets or be called directly from here.

To have direct access to these functions in your Erlang shell, adopt and put the
following lines in the `~/.erlang` file:

    $ cat ~/.erlang
    EHFPath = "{{ABSPATH_TO_EHF}}/ebin",
    code:add_path(EHFPath),
    {ok, L} = file:list_dir(EHFPath),
    [code:load_file(erlang:list_to_atom(filename:basename(F, ".beam"))) || F <- L].
    
This adds the modules to the code path and loads them.

## Categories

Every module is a self-contained categery that I tried to keep close
to the Erlang/OTP names.

## Credits / Copyrights

Either I found them or wrote them myself. So I tried to keep
credits were it's due.

 * Programmatic Bookshelf - Introduction to Erlang
 * Erlang/OTP team
 * Mochiweb
 * Jakob Sievers
 
