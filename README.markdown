yukibot [![Build Status][build-status]][build-log]
=======

Yukibot is ostensibly the IRC bot for the ##compsoc-uk-anime channel
on Freenode, but is basically my pet project to play with IRC in
Haskell.

[build-status]: https://travis-ci.org/barrucadu/yukibot.svg?branch=master
[build-log]:    https://travis-ci.org/barrucadu/yukibot


Building
--------

Build with stack. Only GHC 8 has been tested, it's possible other
versions might work, but you're on your own. The stackage snapshot in
the `stack.yaml` file is the supported set of package versions.

```bash
stack build
```

If you are using [NixOS][], `zlib` is required. The provided
`shell.nix` supplies this; if you are also using [direnv][], allow the
`.envrc` file to set up your environment.

[NixOS]:  https://nixos.org/
[direnv]: https://github.com/direnv/direnv


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
