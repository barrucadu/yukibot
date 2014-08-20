# Yukibot

Yukibot is an IRC bot for the ##compsoc-uk-anime channel on Freenode,
developed as three components:

## IRC Client Library: Integrated Data Thought Entity (IDTE)

Manages a connection to a single IRC network. We want to keep this
simple and have nothing included simply because we intend to make a
bot. This should be able to stand on its own.

 - Handles connecting to a server, optionally with SSL.
 - Handles decoding messages into a nice sum type.
 - Users can register "event handlers" which are fired on receipt of
   messages.
 - Event handlers get a record containing all the IRC state +
   event-specific state.
 - Provides primitives for sending messages (both raw and sum-typey).
 - Handles things like CTCPs and flood prevention behind-the-scenes.

## IRC Bot Library: Boota

This brings in the core functionality of an IRC bot, but as yet
doesn't include anything specific to *our* bot. Furthermore, we want
to implement as few as possible new 'primitives', keeping this layer
simple and flexible.

We may also want to include a few simple commands as examples. Perhaps
a !seen (as it demonstrates state), and an !echo (as a minimal working
example).

 - Abstracts over connecting to multiple networks.
   - Provide a new "register global event handler" function.
   - Provide a list of event handlers which get registered per-network
     upon connection.

 - Manages per-network and per-channel configuration.
   - Channel/Network admins [see permissions plugin].
   - Per-network nick.
   - Per-channel plugins.

 - Adds a concept of "Plugins".
   - Plugins all have mutable state (use acid-state?)
   - May have dependencies (in which case the dependencies must be
     enabled, or an error is raised).
   - A plugin can call functions from another plugin.
   - *Not* event handlers, or even any user-facing thing at all
     necessarily.

 - Adds a "Command Runner" plugin.
   - Hooks into PRIVMSG, matches the command prefix, and runs the
     registered command.
   - *Not* a new 'primitive', this is a plugin built on top of the
     existing event handler architecture.
   - In a /query, no command prefix is needed.
   - Addressing the bot by nick is the same as using a command prefix.
   - Provides a "register command" function.
   - Depends on permissions plugin.

 - Adds a "Permisions" plugin.
   - Users are divided into three classes: User, Channel Admin,
     Network Admin.
   - Channel ops are automatically placed into the Channel Admin
     class.
   - A user+channel+network can be queried, and resolved to a
     permission.
   - Provides a "grant permission" function.

 - Adds "commands".
   - Are plugins
   - Register themselves with the Command Runner.
   - May specify a minimum permission required to execute the command.

## Yukibot

If we've done Boota right, this will just be a collection of commands
and event handlers we register, along with some default configuration.

Things we probably want:

 - !summon (to another channel on the same network, and to a new
   network).
   - Summoning is refused if to a network where there are no known
     network admins.
 - !banish (leave a channel, or network).
   - Banishment is refused if it would result in disconnecting from
     the last network.
 - Link title expander.
 - 4chan/lainchan/whatever thread finder.
 - MAL / Anime-Planet search.
 - Responding to trigger words with some pre-defined phrase.
 - Markov chain.
 - Anime suggester (boku no pico, anyone?)
 - RSS feed watcher (for fansub releases)
 - Currently airing schedule + groups covering each show
 - Quotes.

When implementing all this stuff, we should try to avoid importing
things from IDTE (except types, obviously) and just use Boota, to
reduce coupling and make future change easier.
