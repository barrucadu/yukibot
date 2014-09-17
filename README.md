# Yukibot

Yukibot is ostensibly the IRC bot for the ##compsoc-uk-anime channel
on Freenode, but is basically my pet project to play with IRC in
Haskell.

Note: this relies on the currently un-hackaged [irc-client][], which
is pulled in at the moment as a git submodule.

[irc-client]: https://github.com/barrucadu/irc-client

## Building

    cd yukibot
    cabal sandbox init
    cabal sandbox add-source ../asakura ../irc-client
    cabal install --only-dependencies
    cabal build

## Components

### asakura: IRC bot library

 - Handles connecting to a collection of IRC servers.

 - Basic command and permission modules, upon which more complicated
   constructs can be built.

 - Provides typeclasses for serialisable shared state.

### yukibot: IRC bot

 - See `Yukibot.Plugins` for current functionality.
