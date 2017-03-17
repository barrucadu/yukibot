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
but I use [NixOS][].

```
$ nix-shell shell.nix --indirect --add-root .gcroots/gc
```

This will build everything and drop you in a shell with it all
available. It will also create garbage collector roots in `.gcroots`,
so `nix-collect-garbage` will leave your environment alone (good to
avoid accidentally nuking things).

Firstly you'll want to set the `NIX_GHC_LIBDIR` environment variable
so that `mueval` can find the GHC package database (if you are using
the mueval plugin):

```
$ cat `which ghc`
#! /nix/store/gabjbkwga2dhhp2wzyaxl83r8hjjfc37-bash-4.3-p48/bin/bash -e
export NIX_GHC="/nix/store/g12fa7vc5icmsqj4habg9d2apcpdrfip-ghc-8.0.1-with-packages/bin/ghc"
export NIX_GHCPKG="/nix/store/g12fa7vc5icmsqj4habg9d2apcpdrfip-ghc-8.0.1-with-packages/bin/ghc-pkg"
export NIX_GHC_DOCDIR="/nix/store/g12fa7vc5icmsqj4habg9d2apcpdrfip-ghc-8.0.1-with-packages/share/doc/ghc/html"
export NIX_GHC_LIBDIR="/nix/store/g12fa7vc5icmsqj4habg9d2apcpdrfip-ghc-8.0.1-with-packages/lib/ghc-8.0.1"
exec /nix/store/b0749p1rjpyvq2wyw58x6vgwpkwxcmnn-ghc-8.0.1/bin/ghc "-B$NIX_GHC_LIBDIR" "${extraFlagsArray[@]}" "$@"
```

You'll get different paths. Copy and paste the `NIX_GHC_LIBDIR`
variable into your shell. You can check it worked by evaluating a
simple expression:

```
$ mueval --load-file L.hs --expression=42
... some error messages about missing stuff ...
$ export NIX_GHC_LIBDIR="/nix/store/g12fa7vc5icmsqj4habg9d2apcpdrfip-ghc-8.0.1-with-packages/lib/ghc-8.0.1"
$ mueval --load-file L.hs --expression=42
42
```

Now you're good to go:

```
$ yukibot configuration.toml
```

[NixOS]:  https://nixos.org/


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
