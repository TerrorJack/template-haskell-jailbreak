{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.Haskell.TH.Jailbreak
  ( eval
  ) where

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Convert as GHC
import Data.Char
import qualified DynFlags as GHC
import qualified GHC
import Language.Haskell.TH.Syntax
import qualified Outputable as GHC
import System.Environment
import System.Process
import Unsafe.Coerce

data GHCProperties = GHCProperties
  { ghcPath, ghcLibDir :: FilePath
  , ghcFlags :: [String]
  }

getGHCProperties :: IO GHCProperties
getGHCProperties = do
  ghc_path <- getExecutablePath
  ghc_libdir <-
    takeWhile (not . isSpace) <$> readProcess ghc_path ["--print-libdir"] ""
  ghc_flags <- getArgs
  pure
    GHCProperties
      {ghcPath = ghc_path, ghcLibDir = ghc_libdir, ghcFlags = ghc_flags}

newtype GHCiSession = GHCiSession
  { unGHCiSession :: GHC.Ghc () -> IO ()
  }

newGHCiSession :: GHCProperties -> IO (GHCiSession, IO ())
newGHCiSession GHCProperties {..} = do
  chan <- newEmptyMVar
  _ <-
    forkIO $
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just ghcLibDir) $ do
      dflags <- GHC.getSessionDynFlags
      (dflags', _, _) <-
        GHC.parseDynamicFlagsCmdLine dflags $ map GHC.noLoc ghcFlags
      _ <- GHC.setSessionDynFlags dflags'
      let w = do
            m' <- liftIO $ takeMVar chan
            case m' of
              Just m -> m *> w
              _ -> pure ()
       in w
  pure (GHCiSession $ putMVar chan . Just, putMVar chan Nothing)

getGHCiSession :: Q GHCiSession
getGHCiSession = do
  m <- getQ
  case m of
    Just s -> pure s
    _ -> do
      (s, f) <- runIO $ getGHCProperties >>= newGHCiSession
      addModFinalizer $ runIO f
      putQ s
      pure s

eval :: Q Exp -> Q a
eval m = do
  e' <- m
  case GHC.convertToHsExpr GHC.noSrcSpan e' of
    Left err -> fail $ GHC.showSDocUnsafe err
    Right e -> do
      s <- unGHCiSession <$> getGHCiSession
      runIO $ do
        chan <- newEmptyMVar
        s $ do
          r <- unsafeCoerce <$> GHC.compileParsedExpr e
          liftIO $ putMVar chan $! r
        takeMVar chan
