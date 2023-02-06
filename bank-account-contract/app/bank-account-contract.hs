import Prelude
import Cardano.Api
import BankAccountContract ( bankAccountContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "bank-account-contract.plutus" Nothing bankAccountContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
