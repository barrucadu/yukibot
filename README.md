# Yukibot

Yukibot is ostensibly the IRC bot for the ##compsoc-uk-anime channel
on Freenode, but is basically my pet project to play with IRC in
Haskell.

## Building

### With `stack`

    stack build

### With `cabal`

    cd yukibot
    cabal sandbox init
    cabal sandbox add-source ../asakura
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

Note: The evaluation plugin depends on mueval and show
