{-# LANGUAGE TemplateHaskell #-}

import Foreign
import Language.Haskell.TH.Jailbreak
import Language.Haskell.TH.Syntax

sizeOfType :: Type -> Q Int
sizeOfType t = eval [|sizeOf (undefined :: $(pure t))|]

main :: IO ()
main =
  print
    $(do r <- eval [|2 + 2 :: Int|]
         lift (r :: Int))
