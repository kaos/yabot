yabot
=====

Yet Another Bot, or "why-a-bot(?)" ;c).

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
- `{join, ["#channel", ...]}`
- `{nick, "nickname"}`
- `{pass, "password"}`

The first channel listed in the `join` option is the default channel for sending messages to.

### IRC bridge options

When `yabot_irc` receives a bridged message, there are a few options you can tack on to it:

- `{chan, Channel :: string()}` Send the message to channel. If this option is not given,
  the message is sent to the first channel listed in the `join` client option.

Example bridge config for sending messages to the irc channel `#my-channel`:

```erlang
    {bridges, [{irc, [{chan, "#my-channel"}]}]}
```    


XMPP/Jabber
-----------

For XMPP, there is a `yabot_xmpp` client module, that supports the following options:
- `{jid, "user@example.com"}`
- `{pass, "password"}`
- `{method, <authentication method>}` `password`, `digest` (default), `"PLAIN"`, `"ANONYMOUS"`, `"DIGEST-MD5"`.
- `{room, "chatroom@server.com"}`
- `{nick, "nickname"}`

Note: `yabot_xmpp` does not yet support any bridge options (from [[Common options]]).


Bot
---

The `yabot_bot` can respond to messages it receives from other clients (forwarded using the `bridges` option),
as well as filter/modify them. The filter is applied before bridging and command processing.

Bot options:
- `{nick, "nickname"}`
- `{cmds, [cmd()]}`
- `{filters, [filter()]}`

### Bot Commands

Commands are invoked using the erlang `os:cmd/1` function, passing the message as arguments.

The `cmd()` type has the following spec:

```erlang
-spec cmd() :: {exec, Name :: atom(), Command :: string(), 
    Args :: integer() | {Min :: integer(), Max :: integer()},
    Synopsis :: string(), Help :: string()}.
```

The command string is any executable file on the host system that you want to expose. The output from `stdout` is
routed back as reply to the command.

Args is a constraint on the number of args that is acceptable. This way it is possible to call sensitive commands
that may have a query mode when called without args, for instance.

### Bot Filters

String filters are invoked using the erlang `os:cmd/1` function, piping the message to stdin.

Function filters take the string message as single argument and returns the filtered message as result.
Note, that it is not possible to define functions directly in the config file, so the only way to have function filters
is when the config is generated programmatically.
A couple of dumb example filters are in the `example.config` file.

The `filter()` type has the following spec:

```erlang
-spec filter() :: string() | filter_fun().
-callback filter_fun(Message :: string()) -> Result :: string().
```

### Bot bridge options

When `yabot_bot` receives a bridged message, there are a few options you can tack on to it:

- `async` Don't wait for the bot to process the message. Any replies to the message will be dropped.
- `{allow, Channel :: string() | all}` Filter, forward and process messages received from channel.
- `{deny, Channel :: string() | all}` Do not filter, forward or process messages received from channel.
- `{order, Order :: deny_allow | allow_deny}` How to apply the `allow` and `deny` channel filters.
  The `deny_allow` option will deny messages that match any of the `deny` options *unless* it also 
  matches any of the `allow` options, and allow all messages matching neither `allow` nor `deny`.
  The `allow_deny` option will only allow any messages matching any of the `allow` options, and that
  doesn't match any of the `deny` options, and deny all messages matching neither `allow` nor `deny`.


### Available commands/filters

In `priv/cmds` are some scripts (ok, so far it's just the one) you can use with the yabot bot.

The `translate.escript` file translates messages (ab)using Google Translate.

Example config snippet, as a command in the `cmds` section:
```erlang
{exec, tr, 
  "./priv/cmds/translate.escript", {3, 1000},
  "tr [from <lang-code>|auto] to <lang-code> Text ...",
  "Translate text using Google Translate.\n"
  "From language can be auto detected if left out."
}
```

Example config snippet, as a filter in the `filters` section:
```erlang
  "./priv/cmds/translate.escript to sv"
```

As a funny example, the following config snippet adds a translator bot that translates everything you say to it into Swedish:

```erlang
{bot, yabot_bot, [
  {nick, "chatter-bot"},
  {filters, [
    "./priv/cmds/translate.escript to sv",
    "sed 's/.*/echo \\0/'"
  ]}
]}
```

Using the translate script as filter, and a few bridged bots and some channels/rooms you can setup a multilingual chat,
where each participant can join the channel/room with the language of his/her choice, and the bots will take care of translating
everything that is said between the channels/rooms. I tried it, it's awesome! :D


Common options
--------------

Options that are client agnostic, and should be supported by all clients:

- `{bridges, [Bridge :: client_id() | {client_id(), Opts :: [{Key, Value}]}, ...]}`

The `bridges` option allows messages received in one channel to be forwarded to other channels.
When a message is forwarded to a transport client (such as IRC or XMPP) the message is sent to that channel,
unless it is a private message. Bridges can be chained, so it is possible to have a message take any number
of jumps. This gives the possibility to have filter clients in between two bridged networks.
Bots can act on bridged messages, either on private messages, or public messages that has the bot's name as prefix
(or any other rule the bot may implement). It is bot speicific, really.

The bridge `Opts` values are client specific, and interpreted on the receiving end of the bridge.
See each client for available bridge options.


Run
===

```
./start.sh <config-file>
```

If no config file is specified, `yabot.config` is used if it exists in the current directory.


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/kaos/yabot/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

