# Tezos Multisig Proxy

Utility to sign proxy contract tools with multisig contract.

## Building
- Using _Nix_: `nix-build -A msig-client` or `nix build .#msig-client` if you are have `nix` of supporting _Flakes_
- Using `stack`: `stack build`

When using `stack`, instead of `multisig-client` in description below use `stack exec multisig-client --`.

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
   The only number in storage is `nonce`. For example for storage below `nonce` is equals to 0.
   ```
   Pair { "tz1dPgGGVAyuiKN9nw4jPSGZGiyBt868h8oo" ;
          "tz1evrHo6ZcDjigG6dDuQF3rvVTMMmEeHE9x" ;
          "tz1fdjt7ZdwYkkyHj3fB3YBLsvHv2ZnFSBpc" }
        0
   ```
2. Sign payload with at least `Math.ceil(guard_keys_n / 2)`. In order to obtain signature use
   `sign` subcommand with `call` operation.
   ```bash
   multisig-client sign call --entrypoint 'A' --param 'Unit' \ 
     --target-contract 'KT1KWchg1x4FhGTiML9CEr6AcMuvHwxJNBnp' \
     --multisig-contract 'KT1EiWoQC26VoYtxmDy67sbMrXQTETuWGbVe' \ 
     --nonce 1 --sk-alias msig1 -I '$PATH_TO_TEZOS_CLIENT' > sig1
   ```
   where 
   - `--nonce` is `nonce` extracted from contract storage + 1, 
   - `--entrypoint` is entrypoint you desire to be called by proxy contract
   - `--param` is param which would be passed to contract by proxy contract 
   - `--target-contract` is contract address, which would be called by proxy
   - `--sk-alias` is alias for Secret Key from `tezos-client` wallet you want to sign with
   - `-I` is path to `tezos-client` binary
3. Use `submit` subcommand to submit you payload supplied with enough signatures.
   ```bash
   multisig-client submit call --entrypoint 'A' --param 'Unit' \
     --target-contract 'KT1KWchg1x4FhGTiML9CEr6AcMuvHwxJNBnp' \
     --multisig-contract 'KT1EiWoQC26VoYtxmDy67sbMrXQTETuWGbVe' \
     --nonce 1 --payer-alias alice \
     --signed 'edpkvErJXfHVsMUzJdWhLZqcDBYth6xuRn7XjmkJB2k9ALhHRQpZdR:edsigu5yCm4Sj1axYVo1Lqcfgk71i6crERb8BhaNgbZfQzUkBaawAkaxWpWkMiyK4UCsspQPWEvTSeSpG8iSswytxV6mpg3XKCi' \
     --signed 'edpkvTUVhBAJ5hDMVGUPzyK9vAfRYg6zXXCmiAELksaTxhtnwPv2gR:edsigtmQ4B8hHQ5FmJ5gCi1QQadQdiCfMT8UH66UznqBp9BwtTZCsHGELfTRts4neFTuCD31HTjLoUZzcrUJW1sc7wf6NuYn8oi' \
     --signed 'edpkuTyjn3QvJmk6kuRqDQfRNinbnVYWWZc2HCWojY16N5DtLtejgV:edsigtYJHVN6UwR4uzhBKGjHwm6DLfjuKtnexd3YJgK5rRPA2C272JZg9p6Ac627aspEKtZetguuAETaFoSDQiL9wpfwJNSmrVj' \
     -I '$PATH_TO_TEZOS_CLIENT'
   ```
   This command contains some options from previous step, but also couple new ones inculding: 
   - `--payer-alias` specifies from which account to originate a transaction
   - `--signed` option specifies public key and signature separated with `:`
   
## Rotating keys

The whole process is similar to making proxy calls. Difference is that you would have to specify `rotate-keys` instead of `call` operation in both `sign` and `submit` operations. Also instead of providing data to make a proxy call you have to specify public key hashes of the new multisig committee instead.

Example: 

``` bash
multisig-client sign rotate-keys 
  tz1PJ6tkQqvomEk8p7VKw7FAC5PDxCu76Dz5 \
  tz1ZQ1H7xHjThAyACsJbR4nzqxUr8nwyQaym \
  tz1S3AwY9TzBwtu5YSTGrm5WsTPstLbEks11 \ 
  --multisig-contract 'KT1PUWVBbWpYjh8UfaZMqcmKufHmu4hWnwgs' \
  --nonce 1 --sk-alias msig1 \
  -I '$PATH_TO_TEZOS_CLIENT' > sig1
```
  
