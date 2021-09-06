# aeplugin_dev_mode
AE node plugin for the "ae_dev" (on-demand) consensus mode

This plugin is a simple example of how to plug in development mode logic
in Aeternity nodes. It registers itself to run when `dev_mode` is activated,
and provides a simple Web interface to demonstrate the effects of on-demand
consensus.

## Background

Aeternity is a Proof-of-Work (mining) blockchain, where miners on the network
compete for production of the next block. Mined blocks are broadcast, and
validated by each receiving node. This is safe, but slow and computation-heavy.

When running tests or developing apps on top of the chain, one prefers a mode
where blocks are produced as quickly and cheaply as possible. While this takes
place on a local node, the logic for cryptographically sealing blocks, as well
as the computation requirement protecting against brute-force attacks, can all
be dispensed with.

There is such a consensus mode in Aeternity, called `on_demand`. Once it is
activated, there is no going back, as the blocks produced by it would fail
validation. Also, when `on_demand` consensus is activated, the sync application
in Aeternity is not run. This is for development and testing only.

## Configuration

An example of a (presumably minimal) configuration file can be found in
[examples/devmode.yaml](examples/devmode.yaml).

Some comments below:

First, some system parameters for locating and identifying the dev_mode
plugin. The lib root is a directory under which Erlang applications reside,
much like the Erlang/OTP applications under `(Erlang Top)/lib`.
Only the plugins listed under `system:plugins` will be loaded.

```yaml
system:
    plugin_path: <lib root for plugin (erlang) applications>
    plugins:
        - "aeplugin_dev_mode"
```

Setting the `network_id` to `ae_dev` triggers some useful defaults for
development mode, and also ensures that the node cannot accidentally connect
to the mainnet or testnet.

```yaml
fork_management:
    network_id: ae_dev
```

This setting enables `on_demand` consensus from height zero (the genesis block).
```yaml
chain:
    consensus:
        "0":
            name: "on_demand"
```

Finally, a beneficiary account needs to be defined. This tells the local node,
among other things, that it can mine keyblocks. Also, block rewards will
accumulate on the beneficiary account, and can then be distributed to other
accounts. The specific account below belongs to a key pair hard-coded in
the [aeplugin_dev_mode_handler](src/aeplugin_dev_mode_handler.erl) module.

Setting the reward delay as low as it will go (2), will ensure that rewards
are paid out sooner.

The `strictly_follow_top` option should be set to ensure that each new
microblock is based on the latest microblock top. When blocks are generated
as quickly as possible, there is otherwise a risk that micro-forks occur.
There will be no human-noticeable performance penalty for enabling this option.

```yaml
mining:
    beneficiary: "ak_GLab8McCgXqng1pZbQDmjbCLw6f48qGyP4zWqzqBVnYwdNWVc"
    beneficiary_reward_delay: 2
    strictly_follow_top: true
```

## Demo web interface

There is a very simple web interface to illustrate what the plugin does.
By default, it listens to port 3331, but this can be changed by setting
the OS environment variable `AE_DEVMODE_PORT`.

The demo web page lets you generate a given number of key blocks instantly.
It lists all account balances on the node (presumably, they will be pretty few).
It also provides a way to send tokens between a set of demo accounts - one of
which should be the beneficiary account, thereby accumulating funds.

Whenever a transaction is pushed to the mempool, the plugin will be notified
and emits a microblock.

![aeplugin_dev_mode_screenshot](https://user-images.githubusercontent.com/160216/132022473-15bcd02f-2805-4d90-a0da-ffd530a1a701.png)

