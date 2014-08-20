-- |Functions for encoding and decoding CTCPs.
--
-- CTCP messages are sent as a PRIVMSG or NOTICE, where the first and
-- last characters are 0o001 (SOH), and the escape character is 0o020
-- (DLE).
--
-- Characters are escaped as follows:
--   - 0o000 (NUL) -> 0o020 0o060 ('0')
--   - 0o012 (NL)  -> 0o020 0o156 ('n')
--   - 0o015 (CR)  -> 0o020 0o162 ('r')
--   - 0o020 (DLE) -> 0o020 0o020
--
-- All other appearences of the escape character are errors, and are
-- dropped.
--
-- See http://www.irchelp.org/irchelp/rfc/ctcpspec.html for more
-- details.
module Network.IRC.CTCP
    ( CTCPByteString
    , getUnderlyingByteString
    , toCTCP
    , fromCTCP
    , encodeCTCP
    , decodeCTCP
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

-- *Types

-- |Type representing a CTCP-encoded string. The constructor is NOT
-- exported, making this safe.
newtype CTCPByteString = CBS { _getUnderlyingByteString :: ByteString }

-- |Get the underlying (encoded) bytestring from a CTCP bytestring.
getUnderlyingByteString :: CTCPByteString -> ByteString
getUnderlyingByteString = _getUnderlyingByteString

-- *Encoding and decoding

-- |Turn a command name and arguments into a CTCP-encoded
-- bytestring. This encodes the text with UTF-8. If another encoding
-- is desired, `encodeCTCP` should be used directly.
toCTCP :: Text -> [Text] -> CTCPByteString
toCTCP cmd args = encodeCTCP . encodeUtf8 . T.unwords $ cmd : args

-- |Encode a bytestring with CTCP encoding.
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

-- |Turn a CTCP-encoded bytestring into a command name and
-- arguments. This decodes the next with UTF-8. If another encoding is
-- desired, `decodeCTCP` should be used directly.
fromCTCP :: CTCPByteString -> (Text, [Text])
fromCTCP bs = case splitOn (T.pack " ") . decodeUtf8 . decodeCTCP $ bs of
                (cmd : args) -> (cmd, args)
                _            -> (T.pack "", [])

-- |Decode a CTCP-encoded bytestring
decodeCTCP :: CTCPByteString -> ByteString
decodeCTCP (CBS bs) | isCTCP bs = unescape . B.tail . B.init $ bs
                    | otherwise = bs

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

-- *Utilities

-- |Check if a message body represents a CTCP. CTCPs are at least two
-- bytes long, and start and end with a SOH.
--
-- This is intentionally very lenient, in particular it doesn't check
-- that escape characters are placed correctly. This is because the
-- spec states that misplaced escape characters should be discarded.
isCTCP :: ByteString -> Bool
isCTCP bs = and $ (B.length bs >= 2) : (B.head bs == soh) : (B.last bs == soh) : map (flip B.notElem bs . fst) encodings

-- |Check if a ByteString looks like a CTCP, and if so, wrap it up in
-- the CTCPByteString type.
--
-- This uses `isCTCP`, and so is lenient with escapes.
asCTCP :: ByteString -> Maybe CTCPByteString
asCTCP bs = if isCTCP bs
            then Just $ CBS bs
            else Nothing

-- |Apply one of two functions depending on whether the bytestring is
-- a CTCP or not.
--
-- This uses `isCTCP` under the hood, and so is lenient.
orCTCP :: (ByteString -> a) -> (CTCPByteString -> a) -> ByteString -> a
orCTCP f g bs = maybe (f bs) g (asCTCP bs)
