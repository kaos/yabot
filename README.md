yabot
=====

Yet Another Bot.

Goals: slim and generic core feature set.

Licensed under the Apache 2.0 license (see LICENSE file).


Build
=====

```
make
```


Configure
=========

Create a config file. Look at `example.config` for ideas.

The parts are:

```erlang
[
 {yabot, %% env vars for the yabot application
  [
   {clients, %% list clients to load during start
    [
     {client_id() :: atom(), client_module() :: atom(),
       [client_option() :: {Key :: atom(), Value :: term()}, ...]},
     ...
    ]}
  ]}
].
```


IRC
---

For IRC, there is a `yabot_irc` client module, that supports the following options:
- `{host, "irc.example.com"}`
- `{port, N}`
- `{chan, "#channel"}`
- `{nick, "nickname"}`
- `{pass, "password"}`


XMPP/Jabber
-----------

For XMPP, there is a `yabot_xmpp` client module, that supports the following options:
- `{jid, "user@example.com"}`
- `{pass, "password"}`
- `{method, <authentication method>}` `password`, `digest` (default), `"PLAIN"`, `"ANONYMOUS"`, `"DIGEST-MD5"`.
- `{room, "chatroom@server.com"}`
- `{nick, "nickname"}`


Bot
---

The `yabot_bot` can respond to messages it receives from other clients (forwarded using the `bridges` option).

Bot options:
- `{nick, "nickname"}`
- `{cmds, [cmd()]}`

Nick name the bot should listen to in chat rooms/channels.

Bot Commands
............

The `cmd()` type has the following spec:

```erlang
-spec cmd() :: {exec, Name :: atom(), Command :: string(), Args :: integer() | {Min :: integer(), Max :: integer()},
    Synopsis :: string(), Help :: string()}
```

The command string is any executable file on the host system that you want to expose. The output from `stdout` is
routed back as reply to the command.

Args is a constraint on the number of args that is acceptable. This way it is possible to call sensitive commands
that may have a query mode when called without args, for instance.


Common options
--------------

Options that are client agnostic, and should be supported by all clients:

- `{bridges, [client_id(), ...]}`

The `bridges` option allows messages received in one channel to be forwarded to other channels.
When a message is forwarded to a transport client (such as IRC or XMPP) the message is sent to that channel,
unless it is a private message. Bridges can be chained, so it is possible to have a message take any number
of jumps. This gives the possibility to have filter clients in between two bridged networks.
Bots can act on bridged messages, either on private messages, or public messages that has the bot's name as prefix
(or any other rule the bot may implement). It is bot speicific, really.


Run
===

```
./start.sh <config-file>
```

If no config file is specified, `yabot.config` is used if it exists in the current directory.
