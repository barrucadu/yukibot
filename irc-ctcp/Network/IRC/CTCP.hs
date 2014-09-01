-- |Functions for encoding and decoding CTCPs.
module Network.IRC.CTCP
    ( -- *Types
      CTCPByteString
    , getUnderlyingByteString

    -- *Encoding and decoding
    , toCTCP
    , fromCTCP
    , encodeCTCP
    , decodeCTCP

    -- *Utilities
    , isCTCP
    , asCTCP
    , orCTCP
    ) where

import Data.ByteString    (ByteString, pack, singleton, unpack)
import Data.List          (mapAccumL)
import Data.Maybe         (catMaybes, fromMaybe)
import Data.Text          (Text, splitOn)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Tuple         (swap)

import qualified Data.ByteString as B
import qualified Data.Text       as T

-- |Type representing a CTCP-encoded bytestring.
newtype CTCPByteString = CBS
    { getUnderlyingByteString :: ByteString
    -- ^Get the underlying (encoded) bytestring from a CTCP
    -- bytestring.
    }
    deriving (Eq, Show)

-- |Turn a command name and arguments into a CTCP-encoded bytestring.
--
-- This encodes the text with UTF-8. If another encoding is desired,
-- 'encodeCTCP' should be used directly.
toCTCP :: Text -> [Text] -> CTCPByteString
toCTCP cmd args = encodeCTCP . encodeUtf8 . T.unwords $ cmd : args

-- |Encode a bytestring according to the CTCP spec.
encodeCTCP :: ByteString -> CTCPByteString
encodeCTCP bs = CBS $ B.concat [ singleton soh
                               , escape bs
                               , singleton soh
                               ]

    where escape = B.concatMap escape'
          escape' x = case lookup x encodings of
                        -- If there is an encoding, escape it and use
                        -- that.
                        Just x' -> pack [esc, x']

                        -- Otherwise, just return the original
                        -- character.
                        Nothing -> singleton x

-- |Decode a CTCP-encoded bytestring and turn it into a command name
-- and arguments.
--
-- This decodes the text with UTF-8. If another encoding is desired,
-- 'decodeCTCP' should be used directly.
fromCTCP :: CTCPByteString -> (Text, [Text])
fromCTCP bs = case splitOn (T.pack " ") . decodeUtf8 . decodeCTCP $ bs of
                (cmd : args) -> (cmd, args)
                _            -> (T.pack "", [])

-- |Decode a CTCP bytestring. Extraeneous escapes are dropped.
decodeCTCP :: CTCPByteString -> ByteString
decodeCTCP (CBS bs) = unescape . B.tail . B.init $ bs

    where unescape = pack . catMaybes . snd . mapAccumL step False . unpack

          -- If we fail to find a decoding, ignore the escape.
          step True x = (False, Just . fromMaybe x $ lookup x decodings)

          -- Enter escape mode, this doesn't add a character to the
          -- output.
          step False 0o020 = (True, Nothing)

          step _ x = (False, Just x)

soh :: Integral i => i
soh = 0o001

esc :: Integral i => i
esc = 0o020

encodings :: Integral i => [(i, i)]
encodings = [ (0o000, 0o060)
            , (0o012, 0o156)
            , (0o015, 0o162)
            , (0o020, 0o020)
            ]

decodings :: Integral i => [(i, i)]
decodings = map swap encodings

-- |Check if a bytestring represents a CTCP.
--
-- This is intentionally very lenient, in particular it doesn't check
-- that there are no extra escape characters. This is because the spec
-- states that misplaced escape characters should be discarded by the
-- decoding process.
isCTCP :: ByteString -> Bool
isCTCP bs = and $ (B.length bs >= 2) : (B.head bs == soh) : (B.last bs == soh) : map (flip B.notElem bs . fst) encodings

-- |Check if a bytestring looks like a CTCP, and if so, wrap it up in
-- the 'CTCPByteString' type.
--
-- This uses 'isCTCP', and so is lenient with escapes.
asCTCP :: ByteString -> Maybe CTCPByteString
asCTCP bs = if isCTCP bs
            then Just $ CBS bs
            else Nothing

-- |Apply one of two functions depending on whether the bytestring
-- looks like a CTCP or not.
--
-- This uses 'asCTCP', and so is lenient with escapes.
orCTCP :: (ByteString -> a) -> (CTCPByteString -> a) -> ByteString -> a
orCTCP f g bs = maybe (f bs) g (asCTCP bs)
