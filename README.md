# Yukibot

Yukibot is ostensibly the IRC bot for the ##compsoc-uk-anime channel
on Freenode, but is basically my pet project to play with IRC in
Haskell.

Note: this relies on the currently un-hackaged [irc-conduit][], which
is pulled in at the moment as a git submodule.

[irc-conduit]: https://github.com/barrucadu/irc-conduit

## Building

    cd yukibot
    cabal sandbox init
    cabal sandbox add-source ../asakura ../idte ../irc-conduit
    cabal install --only-dependencies
    cabal build

## Components

### idte: IRC client library

 - Handles a connection to a single IRC server.

 - Manages "event handlers", calling them as appropriate on receipt of
   messages.

 - Provides default event handlers for some common messages (e.g.,
   server PINGs).

 - Executes each event handler in its own thread, and uses a message
   queue to guarantee thread-safe message delivery.

 - Provides a few helper functions for common operations.

### asakura: IRC bot library

 - Handles connecting to a collection of IRC servers.

 - Basic command and permission modules, upon which more complicated
   constructs can be built.

 - Provides typeclasses for serialisable shared state.

### yukibot: IRC bot

 - See `Yukibot.Plugins` for current functionality.
