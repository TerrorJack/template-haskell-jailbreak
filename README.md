# `template-haskell-jailbreak`

[![CircleCI](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master.svg?style=shield)](https://circleci.com/gh/TerrorJack/template-haskell-jailbreak/tree/master)

Black magic to workaround a Template Haskell stage restriction. Doesn't work on Windows yet.

## Demo

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

With `eval`, you are granted the power of jailbreaking. Here comes one example usage: Suppose you want to query the `Storable` instance of a `Type` and calculate its size via calling `sizeOf`. You need something like:

```haskell
sizeOfType :: Type -> Q Int
```

After obtaining the `sizeOfType` result in the `Q` monad, you can generate subsequent `Dec`s you need. What would its implementation look like?

```haskell
sizeOfType :: Type -> Q Int
sizeOfType t = pure $([| sizeOf undefined :: $(pure t) |])
```

Unfortunately `ghc` is protesting:

```
    * GHC stage restriction:
        `t' is used in a top-level splice, quasi-quote, or annotation,
        and must be imported, not defined locally
    * In the untyped splice: $(pure t)
      In the Template Haskell quotation
        [| sizeOf undefined :: $(pure t) |]
      In the untyped splice: $([| sizeOf undefined :: $(pure t) |])
```

It's time to jailbreak!

```haskell
sizeOfType :: Type -> Q Int
sizeOfType t = eval [|sizeOf (undefined :: $(pure t))|]
```

This version works.

There's probably more idiomatic way to achieve this though, do tell me if something ain't right..
