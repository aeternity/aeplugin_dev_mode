## JSON REST API

The dev mode plugin REST interface can be accessed with

All methods are `GET` requests, with URI-encoded parameters.

For all requests, the `pp_json` parameter makes the returned JSON data be pretty-printed.
We will use it in all the examples below.

For all requests, a `chain` object is returned, including the chain height and top hash.
The `status` method returns additional chain parameters.

### Methods

- [Emit microblock](#emit-microblock)
- [Emit keyblock(s)](#emit-keyblocks)
- [Set keyblock interval](#set-keyblock-interval)
- [Set microblock interval](#set-microblock-interval)
- [Auto-emit microblocks](#auto-emit-microblocks)
- [Spend](#spend)
- [Rollback the chain](#rollback-the-chain)
- [Query plugin status](#query-plugin-status)


#### Emit microblock
**Method:** `/emit_mb`
**Parameters:** none

This method instructs the plugin to emit exactly one microblock

**Example:**
```
$ curl -H 'Accept: application/json' http://localhost:3313/emit_mb?pp_json
{
  "chain":{
    "height":1,
    "top_hash":"mh_MPV6aN2rWCMKwKjUHJDfuGY8Kft862RBicfRRVBLFpdMbDR8q"
  },
  "result":"ok"
```

#### Emit keyblocks
**Method:** `/emit_kb`
**Parameters:**
| name    | type    | default |
|---------|---------|---------|
| `n`     | integer | `1`     |

Emit `n` keyblocks (default: 1). Reports the old chain height and the new chain parameters.

**Example:**
```
$curl -H 'Accept: application/json' 'http://localhost:3313/emit_kb?n=3&pp_json'
{
  "chain":{
    "height":4,
    "top_hash":"kh_26HskrvfExthaCaf9qgvN13CG6pPube3LGBGqHmp2DUgcXSryT"
  },
  "old_height":1
```

#### Set keyblock interval
**Method:** `/kb_interval`
**Parameters:**
| name   | type    | default |
|--------|---------|---------|
| `secs` | integer | `0`     |

Sets a keyblock interval of `secs` seconds (`secs=0` turns off)

**Example:**
```
$ curl -H 'Accept: application/json' 'http://localhost:3313/kb_interval?secs=30&pp_json'
{
  "chain":{
    "height":4,
    "top_hash":"kh_26HskrvfExthaCaf9qgvN13CG6pPube3LGBGqHmp2DUgcXSryT"
  },
  "result":"ok"
```

#### Set microblock interval
**Method:** `/mb_interval`
**Parameters:**
| name   | type    | default |
|--------|---------|---------|
| `secs` | integer | `0`     |

Sets a microblock interval of `secs` seconds (`secs=0` turns off)

**Example:**
```
$ curl -H 'Accept: application/json' 'http://localhost:3313/mb_interval?secs=3&pp_json'
{
  "chain":{
    "height":5,
    "top_hash":"kh_2C7SspQrxdEXG47tXr9RM9d5c5aVA275zvytdKcwiVduMQZMcd"
  },
  "result":"ok"
}

[ a few seconds later ]

$ curl -H 'Accept: application/json' 'http://localhost:3313/mb_interval?secs=0&pp_json'
{
  "chain":{
    "height":5,
    "top_hash":"mh_3sRB9d3A9oQAMQyqSMVgMmMbWxRe9VwmjvmabAEdwXb9CkQFp"
  },
  "result":"ok"
```

#### Auto-emit microblocks
**Method:** `/auto_emit_mb`
**Parameters:**
| name        | type                 | default  |
|-------------|----------------------|----------|
| `auto_emit` | string (`on \| off`) | `off`    |

Turns auto-emission of microblocks on or off. If on, microblocks are emitted
automatically whenever transactions are pushed to the mempool. Microblocks
will be emitted until the mempool is empty.

**Example:**
```
$ curl -H 'Accept: application/json' 'http://localhost:3313/auto_emit_mb?auto_emit=off&pp_json'
{
  "chain":{
    "height":5,
    "top_hash":"mh_vMoftTAVvKJhm9noSoK84zAbV69URKP6v649ZvAhACp3ebku3"
  },
  "result":"ok"
```

#### Spend
**Method:** `/spend`
**Parameters (all mandatory):**
| name        | type        |
|-------------|-------------|
| from        | account id  |
| to          | account id  |
| amount      | integer     |

Creates and signs a `spend` transaction, then pushes it to the mempool
If microblock auto-emission is enabled, the transaction will make it automatically
onto the chain.

The `from` account needs to be one of the known demo accounts, for which the
private key is known, and with a sufficient balance to cover `amount`.

The `/status` method can be used to inspect the accounts and balances.

**Example:**
```
$ curl -H 'Accept: application/json' 'http://localhost:3313/spend?from=ak_2a1j2Mk9YSmC1gioUq4PWRm3bsv887MbuRVwyv4KaUGoR1eiKi&to=ak_GLab8McCgXqng1pZbQDmjbCLw6f48qGyP4zWqzqBVnYwdNWVc&amount=500000&pp_json'
{
  "chain":{
    "height":5,
    "top_hash":"mh_vMoftTAVvKJhm9noSoK84zAbV69URKP6v649ZvAhACp3ebku3"
  },
  "result":"ok"

[ Using /status, we can see that the balances have been updated ]

...
  "chain":{
    "all_balances":[
      {
        "balance":500000,
        "public_key":"ak_GLab8McCgXqng1pZbQDmjbCLw6f48qGyP4zWqzqBVnYwdNWVc"
      }
...
```

### Rollback the chain
**Method:** `/rollback`
**Parameters:**
| name       | type       | default |
|------------|------------|---------|
| `height`   | integer    | `""`    |
| `hash`     | block hash | `""`    |

Rolls back the chain database to a given height or block hash.
The block hash can correspond either to a keyblock or a microblock. All subsequent
blocks on the chain will be deleted (including blocks on other forks at greater height).
Transactions in the deleted blocks will also be deleted. Returns the old height, the old
top and current chain information.

**Example:**
```
 curl -H 'Accept: application/json' 'http://localhost:3313/rollback?hash=mh_vMoftTAVvKJhm9noSoK84zAbV69URKP6v649ZvAhACp3ebku3&pp_json'
{
  "chain":{
    "height":5,
    "top_hash":"mh_vMoftTAVvKJhm9noSoK84zAbV69URKP6v649ZvAhACp3ebku3"
  },
  "old_height":6,
  "old_top":"kh_5Rdu5aTK2xdG4kfoyN245RbzcMKL4Ms44tb7qaYuuknmCA2kj"
```

#### Query plugin status
**Method:** `/status`
**Parameters:** none

Returns useful information about the chain and the dev mode plugin.

**Example:**
```
$ curl -H 'Accept: application/json' http://localhost:3313/status?pp_json
{
  "accounts":[
    {
      "private_key":"e6a91d633c77cf5771329d3354b3bcef1bc5e032c43d70b6d35af923ce1eb74dcea7ade470c9f99d9d4e400880a86f1d49bb444b62f11a9ebb64bbcfeb73fef3",
      "public_key":"ak_2a1j2Mk9YSmC1gioUq4PWRm3bsv887MbuRVwyv4KaUGoR1eiKi"
    },
    {
      "private_key":"fbb7adae450f0712b8586546215e899cf1211e1da95044ae13ac70b13c1eee7722d369a81c3f90da1b9445e66ccb3c76bd3043144497bac04db9f83c4991fec1",
      "public_key":"ak_GLab8McCgXqng1pZbQDmjbCLw6f48qGyP4zWqzqBVnYwdNWVc"
    },
    {
      "private_key":"696ab494b4bc4a8c0cdc7c1aafe1a1c0e7845d3d849667827c6c2b1474d955c809814a2c17082abb4ae932f4bdbf5accf99e55f95fb5dddedaf558e9abed5880",
      "public_key":"ak_5Bo28XwNzj95gZSuZXrebxgF2Jpqoy9yfMrjUZJ4ce3P5QGcx"
    },
    {
      "private_key":"ca06321cc4582d662be362e7625a99db25cbd2ba1a8103cba21caee3f8208b2dbabe6e80813107ce80dc7755363e89511337bb914f865ce8ad3c03fd78f035c0",
      "public_key":"ak_2RF847YcuaZNeCtH6mXtc6GmtfZ1L89oTnSiN2yUkMqT4JQLAg"
    }
  ],
  "chain":{
    "all_balances":[
      {
        "balance":327000000000000000,
        "public_key":"ak_2KAcA2Pp1nrR8Wkt3FtCkReGzAi8vJ9Snxa4PcmrthVx8AhPe8"
      },
      {
        "balance":10000000000002673000000000000000,
        "public_key":"ak_2a1j2Mk9YSmC1gioUq4PWRm3bsv887MbuRVwyv4KaUGoR1eiKi"
      }
    ],
    "mempool_height":0,
    "top_hash":"kh_2C7SspQrxdEXG47tXr9RM9d5c5aVA275zvytdKcwiVduMQZMcd",
    "top_height":5
  },
  "devmode_settings":{
    "auto_emit_microblocks":false,
    "keyblock_interval":0,
    "microblock_interval":0
  }
}
```
