# `template-haskell-jailbreak`

[![CircleCI](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master.svg?style=shield)](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master)

Black magic to workaround the Template Haskell stage restriction. Work in progress. Doesn't work on Windows yet.

## Black magic No.1

Retrieve `LocalBuildInfo` in a Template Haskell splice. No need for custom `Setup.hs` script!

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Jailbreak

main :: IO ()
main = print $(lbiQ)
```

## Black magic No.2

Evaluate any Template Haskell `Exp`, any time you want.

```haskell
-- eval :: Q Exp -> Q a

{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Jailbreak
import Language.Haskell.TH.Syntax

main :: IO ()
main =
  print
    $(do r <- eval [|2 + 2 :: Int|]
         lift (r :: Int))
```

With `eval` and `lift`, you are granted the power of `jailbreak`ing:

```haskell
jailbreak :: Lift a => proxy a -> Q Exp -> Q Exp
```

If some stage restriction error pops up with an expression splice, just `jailbreak` it.
