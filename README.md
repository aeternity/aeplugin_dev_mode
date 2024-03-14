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

The plugin uses the `aeplugin_rebar3` rebar plugin. This collects
dependencies from https://github.com/aeternity/aeternity (master).
If you want to build against another version of Aeternity, checkout the
[README for aeplugin_rebar3](https://github.com/aeplugin_rebar3).

Dependencies that are already part of the Aeternity node are given on the
form `{lager, {ae, lager}}`. Other dependencies are declared as usual.

To simply compile the code:
```
$ rebar3 compile
```

To build a plugin archive (will also compile), run the following command:

```
$ rebar3 ae_plugin
```

This should cause generation of a file `_build/default/aeplugin_dev_mode.ez`.
This file is then copied or symlinked into the `AETERNITY_ROOT/plugins/` directory.

## Loading

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
dev_mode:
    keyblock_interval: 0
    microblock_interval: 0
    auto_emit_microblocks: true
```

**NOTE:** In earlier versions of the dev_mode plugin, the block interval and auto-emit
options were given in the plugin config. They have now been moved to the Aeternity node
config.

### Plugin configuration options

#### `workspace_path` (string)
This makes it possible to specify a custom path to where the plugin can
read and write files. The directiory must be read/write accessible.
The default path is `AETERNITY_ROOT/data/aeplugin_dev_mode/`.

### `workspace_name` (string)
This specifies an optional subdirectory under the workspace path.
The default is `""` - i.e. no subdirectory.

### Prefunded accounts settings

| group          | key               | type            |
| ---------------|-------------------|-----------------|
| prefunded      | file              | string          |
| prefunded      | gen               | object          |
| prefunded:gen  | quantity          | integer         |
| prefunded:gen  | balance           | integer         |
| prefunded:gen  | mnemonic          | string          |

When the dev_mode plugin is initialized, it looks for a prefunded accounts file
either specified with the plugin config `prefunded:file`, or as
`WorkSpacePath[/WorkspaceName]/devmode_prefunded_accounts.json`.
If there is no such file, it checks if parameters are provided for automatically
generating accounts. Generation can only proceed if there isn't aleady a genesis
block (the database has not yet been created). If there is no specified prefunded
accounts file, and the chain exists, startup proceeds as normal. This would be the
case when starting in dev mode based on an existing chain.

#### Using multiple workspaces

Here is an example aeternity config, which initializes a workspace, also placing
the chain database in that workspace directory:

```yaml
chain:
  db_path: data/aeplugin_dev_mode/ws1
system:
  dev_mode: true
  plugins:
      -
        name: aeplugin_dev_mode
        config:
          workspace_name: ws1
          prefunded:
            gen:
              quantity: 10
              balance: 1000000
```
The default workspace path of `AE_ROOT/data/aeplugin_dev_mode` is assumed.
Note that if another workspace path is defined in the plugin config, the
`chain:db_path` value needs to be modified to match. The node will expand
all path strings to absolute paths from `AE_ROOT`. It will also create intermediate
directories if possible, and ensure that the user has write access to the target
directory.

In the above case, the plugin will look for the default prefunded accounts file
`data/aeplugin_dev_mode/ws1/devmode_prefunded_accounts.json`. During the
first startup, the file isn't present, so a file is generated using the parameters
in the plugin config `prefunded:gen`. The generated file contains both public
and private keys for each account.

In order to make the accounts be created as the genesis block is created,
a public version of the file is made. The name is the same as the full file,
except that "-PUB" is added just before the extension. This file contains only
the public keys and the account, on the format expected by the node.

After first startup, the contents of the `ws1` directory would be:

```
$ ls data/aeplugin_dev_mode/ws1/
devmode_prefunded_accounts.json      devmode_prefunded_accounts-PUB.json  mnesia/
```

**NOTE:** The node will reconstitute and validate the genesis block at each subsequent
restart, so the prefunded accounts must remain unchanged. If you want to delete and
re-generate, you will also need to delete the database.

### Default settings

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
are defined in the node schema and handled by the node's built-in dev mode emitter.
The settings are automatically checked against the schema at node startup, and processed
by the built-in emitter when `dev_mode` is activated.

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

## Local Docker build

There is a `Dockerfile` in the repository for testing. It uses the `aeternity/aeternity:master` 
docker image. Remember to do `docker pull aeternity/aeternity:master` on occasion, if there
have been updates to the `aeternity` code. You don't have to do it for every build.

Building the docker file (assuming that you have Docker installed):

```
docker pull aeternity/aeternity:master
docker build -t devmode .
```

Running the above image:

```
docker run -t -p 3313:3313 devmode:latest
```

Consult tutorials and documentation on Docker features. Specifically here, you need to use the
`-p` opition to map the HTTP port (default: `3313`) so you can access the REST interface.

You can feed configuration data into the docker container using OS environment variables:

```
docker run -t -p 3313:3313 -e "DEVMODE__PREFUNDED__GEN={\"quantity\":10, \"balance\":100000}" devmode:latest
```

(NOTE that this is similar to setting config data for the Aeternity node using the `AE__` prefix.
The corresponding prefix for the dev mode plugin is `DEVMODE__`.)
