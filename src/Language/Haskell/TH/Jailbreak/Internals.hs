module Language.Haskell.TH.Jailbreak.Internals
  ( getPArgs
  ) where

import Control.DeepSeq
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Unsafe as BS
import Foreign
import Foreign.C

foreign import ccall unsafe "pargs" c_pargs :: Ptr () -> IO CInt

getPArgs :: IO [String]
getPArgs =
  allocaBytes 65536 $ \buf -> do
    r <- c_pargs buf
    case r of
      0 -> do
        bs <- BS.unsafePackCStringLen (castPtr buf, 65536)
        pure $!! map CBS.unpack $ filter (not . BS.null) $ BS.split 0 bs
      _ -> fail "pargs returned error"
