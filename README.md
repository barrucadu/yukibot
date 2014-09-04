# Yukibot

Yukibot is ostensibly the IRC bot for the ##compsoc-uk-anime channel
on Freenode, but is basically my pet project to play with IRC in
Haskell.

## Building

    cd yukibot
    cabal sandbox init
    cabal sandbox add-source ../asakura ../idte ../irc-conduit
    cabal install --only-dependencies
    cabal build

## Components

### irc-conduit: IRC message encoding and decoding + networking

 - Provides [conduits][conduit] for translating bytestrings into
   "events", and "messages" into bytestrings.

 - Provides a sum type for all IRC messages you're likely to want to
   deal with in a client.

 - Provides two helper functions for connecting to IRC servers
   directly.

 - Manages flood protection when connecting to a server directly.

[conduit]: https://hackage.haskell.org/package/conduit

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
