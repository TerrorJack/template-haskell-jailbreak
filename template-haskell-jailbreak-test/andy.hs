{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent
import Control.Monad.IO.Class
import qualified GHC
import Language.Haskell.TH.Jailbreak.Internals
import Unsafe.Coerce

main :: IO ()
main = do
  (f, g) <- newGHCiSession $(lbiQ)
  chan <- newEmptyMVar
  f $ do
    GHC.setContext
      [GHC.IIDecl $ GHC.simpleImportDecl $ GHC.mkModuleName "Prelude"]
    r <- unsafeCoerce <$> GHC.compileExpr "2+2 :: Int"
    liftIO $ putMVar chan $! r
  g
  r <- takeMVar chan
  print (r :: Int)
