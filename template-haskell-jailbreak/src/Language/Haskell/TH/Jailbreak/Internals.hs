{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Jailbreak.Internals
  ( lbiQ
  ) where

import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Foldable
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Foreign
import Foreign.C
import Language.Haskell.TH.Syntax
import System.IO.Unsafe

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
