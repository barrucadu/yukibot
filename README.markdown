yukibot [![Build Status][build-status]][build-log]
=======

Yukibot is ostensibly the IRC bot for the ##compsoc-uk-anime channel
on Freenode, but is basically my pet project to play with IRC in
Haskell.

[build-status]: https://travis-ci.org/barrucadu/yukibot.svg?branch=master
[build-log]:    https://travis-ci.org/barrucadu/yukibot


Usage
-----

There are cabal files, so you can build this with cabal if you want,
but I use [nix][].

### The Scripted Way

```
$ run-yukibot /path/to/configuration/file
```

This will build everything with nix and start a `tmux` session called
"yukibot", running yukibot with the given configuration file.

Calling the script a second time will build the new yukibot, kill the
old one, and start the new one. As the build is done first, there
should be minimal downtime during the switch.

```
$ kill-yukibot
```

This will send a C-c to a `tmux` session called "yukibot".

### The Manual Way

```
$ ./gen-package-list.sh
$ nix-shell
```

This will build everything and drop you in a shell with it all
available. Now you're good to go:

```
$ yukibot configuration.toml
```

[nix]: http://nixos.org/nix/


Configuration
-------------

Configuration files are in Tom's Obvious, Minimal Language ([TOML][]).
See the `example-configuration.toml` file.

[TOML]: https://github.com/toml-lang/toml


Organisation
------------

There are four components in a bot:

 - The *core*, which contains all the logic and is provided by the
   `yukibot-core` package.

- The *backends*, which provide a uniform interface to the actual
   backend in use, such as a connection to an IRC server. These are
   provided by the `yukibot-backend-*` packages.

- The *plugins*, which provide *monitors*, run on every message from
   the backend; and *commands*, activated by a prefix key followed by
   a "verb". These are provided by the `yukibot-plugin-*` packages.

- The *executable*, which loads the backends and plugins, and passes
   this initial state off to the *core* along with the configuration.
   An example bot is provided by the `yukibot` package.
