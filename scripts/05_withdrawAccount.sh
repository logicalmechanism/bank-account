#!/bin/bash
set -e

export CARDANO_NODE_SOCKET_PATH=$(cat path_to_socket.sh)
cli=$(cat path_to_cli.sh)
testnet_magic=$(cat testnet.magic)

# get params
${cli} query protocol-parameters --testnet-magic ${testnet_magic} --out-file tmp/protocol.json

# collat info
collat_address=$(cat wallets/collat-wallet/payment.addr)
collat_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/collat-wallet/payment.vkey)

# script
lock_path="../bank-account-contract/bank-account-contract.plutus"
script_address=$(${cli} address build --payment-script-file ${lock_path} --testnet-magic ${testnet_magic})

payer_address=$(cat wallets/seller-wallet/payment.addr)
payer_pkh=$(${cli} address key-hash --payment-verification-key-file wallets/seller-wallet/payment.vkey)

# get script info
echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" --arg payerPkh "${payer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $payerPkh) | .key | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

CURRENT_VALUE=$(jq -r --arg payerPkh "${payer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $payerPkh) | .value.value.lovelace' tmp/script_utxo.json)
THRESHOLD=$(jq -r --arg payerPkh "${payer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $payerPkh) | .value.inlineDatum.fields[1].fields[0].int' tmp/script_utxo.json)
MINIMUM=$(jq -r --arg payerPkh "${payer_pkh}" 'to_entries[] | select(.value.inlineDatum.fields[0].fields[0].bytes == $payerPkh) | .value.inlineDatum.fields[1].fields[1].int' tmp/script_utxo.json)

# check threshold
if [[ $CURRENT_VALUE -lt $THRESHOLD ]] ; then
    echo -e "\n \033[0;31m Balance Must Be Greater Than Account Threshold \033[0m \n";
    exit
fi

# empty args
if [[ $# -eq 0 ]] ; then
    echo -e "\n \033[0;31m Please Enter A Withdraw Amount In Lovelace \033[0m \n";
    exit
fi

# arg equals 0
if [[ ${1} -eq 0 ]] ; then
    echo -e "\n \033[0;31m Withdraw Must Be Greater Than Zero \033[0m \n";
    exit
fi

newAmount=$((${CURRENT_VALUE} - ${1}))

# check threshold
if [[ $newAmount -lt $MINIMUM ]] ; then
    echo -e "\n \033[0;31m Balance Must Be Greater Than Account Minimum \033[0m \n";
    exit
fi

# update the starting lock time
variable=${1}; jq --argjson variable "$variable" '.fields[0].fields[0].int=$variable' data/redeemer/withdraw-redeemer.json > data/redeemer/withdraw-redeemer-new.json
mv data/redeemer/withdraw-redeemer-new.json data/redeemer/withdraw-redeemer.json

script_address_out="${script_address} + ${newAmount}"
echo "Script OUTPUT: "${script_address_out}

#
# exit
#
echo -e "\033[0;36m Gathering Collateral UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${collat_address} \
    --out-file tmp/collat_utxo.json

TXNS=$(jq length tmp/collat_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${collat_address} \033[0m \n";
   exit;
fi
collat_utxo=$(jq -r 'keys[0]' tmp/collat_utxo.json)


echo -e "\033[0;36m Gathering Seller UTxO Information  \033[0m"
${cli} query utxo \
    --testnet-magic ${testnet_magic} \
    --address ${payer_address} \
    --out-file tmp/payer_utxo.json

TXNS=$(jq length tmp/payer_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${payer_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/payer_utxo.json)
payer_tx_in=${TXIN::-8}

# get script info
echo -e "\033[0;36m Gathering Script UTxO Information  \033[0m"
${cli} query utxo \
    --address ${script_address} \
    --testnet-magic ${testnet_magic} \
    --out-file tmp/script_utxo.json
# transaction variables
TXNS=$(jq length tmp/script_utxo.json)
if [ "${TXNS}" -eq "0" ]; then
   echo -e "\n \033[0;31m NO UTxOs Found At ${script_address} \033[0m \n";
   exit;
fi
alltxin=""
TXIN=$(jq -r --arg alltxin "" 'keys[] | . + $alltxin + " --tx-in"' tmp/script_utxo.json)
script_tx_in=${TXIN::-8}

# get script reference tx
script_ref_utxo=$(${cli} transaction txid --tx-file tmp/tx-reference-utxo.signed)

echo -e "\033[0;36m Building Tx \033[0m"
FEE=$(${cli} transaction build \
    --babbage-era \
    --protocol-params-file tmp/protocol.json \
    --out-file tmp/tx.draft \
    --change-address ${payer_address} \
    --tx-in-collateral="${collat_utxo}" \
    --tx-in ${payer_tx_in} \
    --tx-in ${script_tx_in} \
    --spending-tx-in-reference="${script_ref_utxo}#1" \
    --spending-plutus-script-v2 \
    --spending-reference-tx-in-inline-datum-present \
    --spending-reference-tx-in-redeemer-file data/redeemer/withdraw-redeemer.json \
    --tx-out="${script_address_out}" \
    --tx-out-inline-datum-file data/datum/account-datum.json \
    --required-signer-hash ${payer_pkh} \
    --required-signer-hash ${collat_pkh} \
    --testnet-magic ${testnet_magic})

IFS=':' read -ra VALUE <<< "${FEE}"
IFS=' ' read -ra FEE <<< "${VALUE[1]}"
FEE=${FEE[1]}
echo -e "\033[1;32m Fee: \033[0m" $FEE
#
# exit
#
echo -e "\033[0;36m Signing \033[0m"
${cli} transaction sign \
    --signing-key-file wallets/seller-wallet/payment.skey \
    --signing-key-file wallets/collat-wallet/payment.skey \
    --tx-body-file tmp/tx.draft \
    --out-file tmp/tx.signed \
    --testnet-magic ${testnet_magic}
#    
# exit
#
echo -e "\033[0;36m Submitting \033[0m"
${cli} transaction submit \
    --testnet-magic ${testnet_magic} \
    --tx-file tmp/tx.signed
