{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Jailbreak.Internals
import Language.Haskell.TH.Syntax

main :: IO ()
main =
  print
    $(do r <- eval [|2 + 2 :: Int|]
         lift (r :: Int))
