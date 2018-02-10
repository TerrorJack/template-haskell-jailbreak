{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Jailbreak.Internals

main :: IO ()
main = print $(lbiQ)
