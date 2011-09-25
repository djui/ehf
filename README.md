# Tulib - Erlang Helper Function Library

This is a collection of useful Erlang functions that I collected over
time. It can be used as snippets or be called directly from here.

To have direct access to these functions in your Erlang shell, adopt and put the
following lines in the `~/.erlang` file:

    $ cat ~/.erlang
    TulibPath = "{{ABSPATH_TO_TULIB}}/ebin",
    code:add_path(TulibPath),
    {ok, L} = file:list_dir(TulibPath),
    [code:load_file(erlang:list_to_atom(filename:basename(F, ".beam"))) || F <- L].

This adds the modules to the code path and loads them.

## Categories

Every module is a self-contained categery that I tried to keep close
to the Erlang/OTP names.

### tulib\_assert

### tulib\_beam\_lib

### tulib\_deployer

### tulib\_dict

### tulib\_erlang

### tulib\_file

### tulib\_inet

### tulib\_lists

### tulib\_math

### tulib\_message

### tulib\_os

### tulib\_process

### tulib\_reloader

### tulib\_shell

### tulib\_string


## Credits / Copyrights

Either I found them or wrote them myself. So I tried to keep
credits were it's due.

 * Programmatic Bookshelf - Introduction to Erlang
 * Mochiweb
 * Jakob Sievers
