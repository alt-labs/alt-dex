## AltLabs — DEX PoC

Simple CLI to serialize plutus scripts, which should be imported into the frontend `web`which is respoinsible for signing messages with the Nami Wallet. This repo contains all the separate pieces that are required to perform a full roundtrip:

Plutus Contract Serialise ► Deploy Script ► User Consume (browser) ►  Sign/Submit via Nami Wallet

###### Enter nix shell

```bash
nix-shell
```

Prerequisite env variables for cardano node, for example:

```bash
export CARDANO_NODE_SOCKET_PATH=$HOME/altlabs/alonzo/remote_sockets/node-server.sock
export TESTNET_MAGIC_NUM=1097911063
export CARDANO_CLI="cardano-cli"
```

###### Verify the cli connects proper to the socket from `CARDANO_NODE_SOCKET_PATH`

```bash
cabal build adex && cabal exec -- adex conn:check	
```

###### Use the cli to check if the node is in sync

```bash
cabal build adex && cabal exec -- adex sync:status
```

Should output

```
Up to date
Flags: []
Files: ["sync:status"]
             /\   |  _|_    |    _.  |_    _
DEX PoC by  /--\  |   |_    |_  (_|  |_)  _>

[Found socket] = /Users/igor/altlabs/alonzo/remote_sockets/node-server.sock
	-Last Slot Synced : 48048498 (ᛝ 18.946421s s)
	-Local Time : 2022-01-17T11:08:52.946421Z
	-Last Sync  : 2022-01-17T11:08:34Z
```

###### To generate a minting contract locked to a specific UTxO run:

```bash
cabal build adex && cabal exec -- adex gens:mint "86616b7707fc9e08ff76e54b5e728933f59c5f16b826174f45bdabbc02ad0de9#0"
```

Where `86616b7707fc9e08ff76e54b5e728933f59c5f16b826174f45bdabbc02ad0de9` is the output address and `#0` represents the output index. This command would generate a plutus script file called `altswap-tokens.plutus`

###### Once the minting contract is created, you have to make swapping factory scripts, by invoking the following:

```bash
cabal build adex && cabal exec -- adex gens:factory <UTxO>
```

Where `<UTxO>` is the same format `hash#index` as in the minting example.

This would generate 2 more plutus script files called `altswap-nft.plutus` `altswap.plutus`

These two files can then be fed into the frontend which is contained in. the `web` directory and/or be invoked using `cardano-cli` or premade bash scripts that use `cardano-cli` 

The whole roundtrip is presented in the voice-over screen recordings.
