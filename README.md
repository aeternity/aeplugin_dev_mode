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

## Building

The basic plugin uses some components that the Aeternity source depends on.
To ensure that the same versions are used, check out the Aeternity source
from [github](https://github.com/aeternity/aeternity), then set the OS
environment variable `AE_ROOT` to point to the top `aeternity/` directory.
The `rebar.config.script` logic in `aeplugin_dev_mode` will then fetch
the correct versions for all dependencies listed with only the component
name, e.g. `{deps, [lager, cowboy]}`.

Example:
```
$ export AE_ROOT=~/dev/aeternity
$ rebar3 compile
```

Note that (for now), the compiled code ends up under `_build/default/lib/`,
so you can symlink or copy `_build/default/lib/aeplugin_dev_mode` into the
Aeternity plugin lib root (see below).

## Loading

After compiling, copy or link the application directory found at
`_build/default/lib/aeplugin_dev_mode` into the AE node's plugin lib
root. If not otherwise configured, this defaults to `$AETERNITY_TOP/plugins/`.
After this, the node can be started either with a configuration as suggested
below, or most simply by providing some OS environment variables, for example:

```
AE__SYSTEM__DEV_MODE=true \
  AE__SYSTEM__PLUGINS='[{"name":"aeplugin_dev_mode"}]' \
  bin/aeternity console
```

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
    dev_mode: true
    plugins:
        -
          name: aeplugin_dev_mode
          config:
            keyblock_interval: 0
            microblock_interval: 0
            auto_emit_microblocks: true
```

Note that the `config` attribute can be left out. The values in the example
are the default values, except for `auto_emit_microblocks`, which defaults to `false`.

Setting `dev_mode: true` instructs the node to set sensible defaults for development,
unless those parameters are explicitly set to other values. These defaults are:

| group             | key                        | value                   |
|-------------------|----------------------------|-------------------------|
| `fork_management` | `network_id`               | `ae_dev`                |
| `chain`           | `consensus`                | `0 : {name: on_demand}` |
| `mining`          | `beneficiary_reward_delay` | `2`                     |
| `mining`          | `strictly_follow_top`      | `true`                  |
| `mining` | `beneficiary` | `ak_2a1j2Mk9YSmC1gioUq4PWRm3bsv887MbuRVwyv4KaUGoR1eiKi` |

The beneficiary account is a keypair created for testing purposes, which can be
accessed from the node using `aecore_env:patron_keypair_for_testing()`. It is
automatically loaded during dev mode and testing as a pre-configured account, loaded
with 10<sup>32</sup> AE.

Note that setting the `network_id` to `ae_dev` activates some defaults in itself -
mainly: the `on_demand` consensus mode.

The parameters `keyblock_interval`, `microblock_interval` and `auto_emit_microblocks`
are defined in the schema [priv/aeplugin_dev_mode_config_schema.json](priv/aeplugin_dev_mode_config_schema.json). The settings are automatically checked against the schema when the plugin starts.

Setting the reward delay as low as it will go (`2`), will ensure that rewards
are paid out sooner, which can be useful to generate funds in the beneficiary
account by producing keyblocks.

The `strictly_follow_top` option should be set to ensure that each new
microblock is based on the latest microblock top. When blocks are generated
as quickly as possible, there is otherwise a risk that micro-forks occur.
There will be no human-noticeable performance penalty for enabling this option.

## Demo web interface

There is a very simple web interface to illustrate what the plugin does.
By default, it listens to port 3313, but this can be changed by setting
the OS environment variable `AE_DEVMODE_PORT`.

The demo web page lets you generate a given number of key blocks instantly.
It lists all account balances on the node (presumably, they will be pretty few).
It also provides a way to send tokens between a set of demo accounts - one of
which should be the beneficiary account, thereby accumulating funds.

Whenever a transaction is pushed to the mempool, the plugin will be notified
and emits a microblock.

![devmode-ui](https://user-images.githubusercontent.com/160216/132554293-36d90780-af3b-4967-b39b-adc49f4f9bf3.png)

## Demo REST interface

The REST interface can return JSON data. See the [JSON REST API documentation](doc/json_api.md)
for details.

For the purposes of the demo web interface, the following primitive REST API was
added:

* `/emit_mb` - Emit a microblock
* `/emit_kb?n=<N>` - Emit N keyblocks
* `/kb_interval?secs=<S>` - Set a keyblock interval of S seconds (`secs=0` turns off)
* `/mb_interval?secs=<S>` - Set a microblock interval of S seconds (`secs=0` turns off)
* `/auto_emit_mb?auto_emit=on` - Turn on auto-emission of microblocks on tx push
* `/auto_emit_mb?auto_emit=off` - Turn off auto-emission of microblocks on tx push

Note that when `auto_emit` is on, the devmode consensus logic will interleave microblocks
and keyblocks (one microblock followed by one keyblocks) until all expected transactions
are on-chain.
