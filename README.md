# ethtxd
Lightweight Ethereum Tx Decoder with a built in REST API.

## Whats this?

A simple tool with an API to extract out Ethereum call traces

## Install

```bash
make build

# Replace with
cp dist-newstyle/build/x86_64-linux/ghc-<version>/ethtxd-<version>/x/ethtxd/build/ethtxd/ethtxd ~/.local/bin/ethtxd
```

## Usage

```bash
# Terminal 1
ethtxd -p 3000 -r https://mainnet.infura.io/v3/<PROJECT_ID>

# Terminal 2
curl localhost:3000/tx/<TX_HASH>
```

### Example Return

Note that you can compose this tool with [4byte.directory](https://www.4byte.directory/) or [abi-decoder](https://github.com/ConsenSys/abi-decoder) to decode the function signatures and event topics.

```json
{
  "tag": "SuccessResponse",
  "txHash": "0xc7963f40f6e62de89ca32e4b39d4a26e354ed743e0e922ad1917e3d90b6bbc2e",
  "traces": [
    {
      "tag": "TxCall",
      "callTarget": "0x95c4b6c7cff608c0ca048df8b81a484aa377172b",
      "callSigBytes": "0xa9059cbb",
      "callData": "0x0000000000000000000000009389e143dff86096766cd9ff82198857745a8d7b00000000000000000000000000000000000000000000000863a750c3291ed23b",
      "callTrace": [
        {
          "tag": "TxEvent",
          "eventBytes": "0x00000000000000000000000000000000000000000000000863a750c3291ed23b",
          "eventTopics": [
            "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
            "0x3a22df48d84957f907e67f4313e3d43179040d6e",
            "0x9389e143dff86096766cd9ff82198857745a8d7b"
          ]
        },
        {
          "tag": "TxReturn",
          "returnData": "0x0000000000000000000000000000000000000000000000000000000000000001"
        }
      ]
    },
    {
      "tag": "TxEvent",
      "eventBytes": "0x00000000000000000000000000000000000000000000000863a750c3291ed23b",
      "eventTopics": [
        "0x7084f5476618d8e60b11ef0d7d3f06914655adb8793e28ff7f018d4c76d505d5",
        "0x9389e143dff86096766cd9ff82198857745a8d7b"
      ]
    }
  ]
}
```

## Build

```bash
# Install Nix
sh <(curl -L https://nixos.org/nix/install) --daemon

nix-env -iA cachix -f https://cachix.org/api/v1/install

cachix use dapp
cachix use ethtxd

make release
```

## Development

We're using VSCode with the [vscode-haskell](https://github.com/haskell/vscode-haskell/) plugin. The supplied `nix-shell` should have all the dependencies installed by default.

```bash
git clone git@github.com:kendricktan/ethtxd.git
cd ethtxd
nix-shell
code .
```

```bash
# Alternatively
make repl # For a ghci repl
```