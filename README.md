# The Bank Account Contract

A contract to handle micro-payments of lovelace. The contract is not designed for account to account lovelace transfers but rather for small amounts of lovelace to increment an already existing account. This contract attemtps to solve the micro-payment problem by allowing anyone to deposit lovelace into the account by using the ada inside the account as the minimum required lovelace for a transaction. Owner's of the account may withdraw their funds at any time and if required may close their accounts.

The micro-payment problem arises from token marketplaces trying to respect CIP-27, royalty payments token projects. For many token sales, royalty payments are too small to be able to be paid, resulting in incorrect royalty payments by either complete ignorance to payments less than the minimum lovelace or over payments by assuming the minimum require lovelace for a transaction. This contract will allow token project creators to have personal accounts that are able to receive arbitrary amounts of lovelace from anyone, providing a means for small royalty payments from marketplaces.

The contract is designed to be coupled into another smart contract. The marketplace may use the bank account contract as a way to facilitate payments to token project creators that are too small for a single tx out inside a transaction.


