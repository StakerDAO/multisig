# Tezos Multisig Proxy

**TODO** Write short description here

## Building
- Using Nix: `nix-build -A msig-client` or `nix build .#msig-client` if you are have `nix` of supporting _Flakes_

## Deploying contract

Precodition: `tezos-client` is available on `$PATH`.

1. Print contract code using `print-contract` and save it: `multisig-client print-contract > contract.tz`
2. Print inital storage with specified guard keys using `print-storage` command and save it: 
    ```bash
    multisig-client print-storage \
    tz1evrHo6ZcDjigG6dDuQF3rvVTMMmEeHE9x \
    tz1dPgGGVAyuiKN9nw4jPSGZGiyBt868h8oo \
    tz1fdjt7ZdwYkkyHj3fB3YBLsvHv2ZnFSBpc > init.tz
    ```
3. Deploy using `tezos-client`: 
   ```bash
   tezos-client originate contract 'multisig-proxy' transferring 0 from 'my-account' \ 
   running contract.tz --init 'paste from init.tz here'
   ```
   Extra run would probably be needed to correct for `--burn-cap`

## Running proxy calls

1. Obtain contract nonce from contract storage: 
   ```bash
   tezos-client get contract storage for 'multisig-proxy' 
   ```
   The only number in storage is `nonce`. For example for storage below `nonce` is equals to 0,
   ```
   Pair { "tz1dPgGGVAyuiKN9nw4jPSGZGiyBt868h8oo" ;
          "tz1evrHo6ZcDjigG6dDuQF3rvVTMMmEeHE9x" ;
          "tz1fdjt7ZdwYkkyHj3fB3YBLsvHv2ZnFSBpc" }
        0
   ```
2. 
