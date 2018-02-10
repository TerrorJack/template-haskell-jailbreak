{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Jailbreak
  ( lbiQ
  , eval
  ) where

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Convert as GHC
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Foldable
import qualified Data.Map as M
import Distribution.Simple
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import qualified DynFlags as GHC
import Foreign
import Foreign.C
import qualified GHC
import Language.Haskell.TH.Syntax
import qualified Outputable as GHC
import System.IO.Unsafe
import Unsafe.Coerce

foreign import ccall unsafe "pargs" c_pargs :: Ptr () -> IO CInt

getLBIPath :: IO String
getLBIPath =
  allocaBytes 65536 $ \buf -> do
    r <- c_pargs buf
    case r of
      0 -> do
        bs <- BS.unsafePackCStringLen (castPtr buf, 65536)
        findDistPrefOrDefault $
          case find (BS.isPrefixOf "--builddir") $ BS.split 0 bs of
            Just r' ->
              case r' `CBS.index` 10 of
                '=' -> toFlag $ CBS.unpack $ last $ CBS.split '=' r'
                ' ' -> toFlag $ CBS.unpack $ last $ CBS.split ' ' r'
                _ -> NoFlag
            _ -> NoFlag
      _ -> fail "pargs returned error"

getLBI :: IO LocalBuildInfo
getLBI = getLBIPath >>= getPersistBuildConfig

lbiQ :: Q Exp
lbiQ = do
  buf <- encode <$> runIO getLBI
  [|unsafePerformIO $ do
      bs <-
        BS.unsafePackAddressLen
          $(lift $ LBS.length buf)
          $(pure $ LitE $ StringPrimL $ LBS.unpack buf)
      pure ((decode $ LBS.fromStrict bs) :: LocalBuildInfo)|]

adjustDynFlags :: LocalBuildInfo -> GHC.DynFlags -> GHC.DynFlags
adjustDynFlags lbi dflags =
  dflags
    { GHC.packageDBFlags =
        let single (SpecificPackageDB db) = GHC.PackageDB $ GHC.PkgConfFile db
            single GlobalPackageDB = GHC.PackageDB GHC.GlobalPkgConf
            single UserPackageDB = GHC.PackageDB GHC.UserPkgConf
            isSpecific (SpecificPackageDB _) = True
            isSpecific _ = False
         in reverse $
            case withPackageDB lbi of
              (GlobalPackageDB:UserPackageDB:dbs)
                | all isSpecific dbs -> fmap single dbs
              (GlobalPackageDB:dbs)
                | all isSpecific dbs -> GHC.NoUserPackageDB : fmap single dbs
              dbs -> GHC.ClearPackageDBs : fmap single dbs
    }

ghcLibDir :: LocalBuildInfo -> FilePath
ghcLibDir lbi = compilerProperties (compiler lbi) M.! "LibDir"

newGHCiSession :: LocalBuildInfo -> IO (GHC.Ghc () -> IO (), IO ())
newGHCiSession lbi = do
  chan <- newEmptyMVar
  _ <-
    forkIO $
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just $ ghcLibDir lbi) $ do
      dflags <- GHC.getSessionDynFlags
      _ <- GHC.setSessionDynFlags $ adjustDynFlags lbi dflags
      let w = do
            m' <- liftIO $ takeMVar chan
            case m' of
              Just m -> m *> w
              _ -> pure ()
       in w
  pure (putMVar chan . Just, putMVar chan Nothing)

getGHCiSession :: Q (GHC.Ghc () -> IO ())
getGHCiSession = do
  m <- getQ
  case m of
    Just s -> pure s
    _ -> do
      (s, f) <- runIO $ getLBI >>= newGHCiSession
      addModFinalizer $ runIO f
      putQ s
      pure s

eval :: Q Exp -> Q a
eval m = do
  e' <- m
  case GHC.convertToHsExpr GHC.noSrcSpan e' of
    Left err -> fail $ GHC.showSDocUnsafe err
    Right e -> do
      s <- getGHCiSession
      runIO $ do
        chan <- newEmptyMVar
        s $ do
          r <- unsafeCoerce <$> GHC.compileParsedExpr e
          liftIO $ putMVar chan $! r
        takeMVar chan
