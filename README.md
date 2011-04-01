# EHF - Erlang Helper Function Toolbox

This is a collection of useful Erlang functions that I collected over
time. It can be used as snippets or be called directly from here.

To have direct access to these functions in your Erlang shell, adopt and put the
following lines in the `~/.erlang` file:

    $ cat ~/.erlang
    EHF_Path = "{{ABSPATH_TO_EHF}}/ebin",
    code:add_path(EHF_Path),
    {ok, L} = file:list_dir(EHF_Path),
    [code:load_file(erlang:list_to_atom(filename:basename(F, ".beam"))) || F <- L].

This adds the modules to the code path and loads them.

## Categories

Every module is a self-contained categery that I tried to keep close
to the Erlang/OTP names.

### ehf\_assert

### ehf\_beam\_lib

### ehf\_deployer

### ehf\_dict

### ehf\_erlang

### ehf\_file

### ehf\_inet

### ehf\_lists

### ehf\_math

### ehf\_message

### ehf\_os

### ehf\_process

### ehf\_reloader

### ehf\_shell

### ehf\_string


## Credits / Copyrights

Either I found them or wrote them myself. So I tried to keep
credits were it's due.

 * Programmatic Bookshelf - Introduction to Erlang
 * Mochiweb
