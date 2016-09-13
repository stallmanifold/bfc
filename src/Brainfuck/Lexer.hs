{-# LANGUAGE TypeFamilies #-}

module Brainfuck.Lexer
    (
        BFToken(..),
        BFTokenStream(..),
        bfTokenStream,
        minimize
    ) where

import Data.ByteString          as BS
import Data.ByteString.Char8    as BSC8
import Data.ByteString.Internal (c2w, w2c)
import Data.Word
import Data.Proxy
import Data.List                as L
import Data.Maybe
import Text.Megaparsec.Prim
import Text.Megaparsec.Pos



data BFToken = BFRightBracket -- ">"
             | BFLeftBracket  -- "<"
             | BFPlusSign     -- "+"
             | BFMinusSign    -- "-"
             | BFDot          -- "."
             | BFComma        -- ","
             | BFLeftBrace    -- "["
             | BFRightBrace   -- "]"
             deriving(Eq, Ord, Show)


newtype BFTokenStream = BFTokenStream [BFToken]


instance Stream BFTokenStream where
    type Token BFTokenStream = BFToken

    uncons (BFTokenStream [])     = Nothing
    uncons (BFTokenStream (t:ts)) = Just (t, BFTokenStream ts)

    updatePos = const bfUpdatePos


toInstr :: Word8 -> Maybe BFToken
toInstr w = case w2c w of
    '>' -> Just BFRightBracket
    '<' -> Just BFLeftBracket
    '+' -> Just BFPlusSign
    '-' -> Just BFMinusSign
    '.' -> Just BFDot
    ',' -> Just BFComma
    '[' -> Just BFLeftBrace
    ']' -> Just BFRightBrace
    _   -> Nothing


bfUpdatePos :: Pos -> SourcePos -> BFToken -> (SourcePos, SourcePos)
bfUpdatePos tabWidth oldPos@(SourcePos name line col) token = (oldPos, newPos)
    where
        c = unPos col
        newCol = unsafePos (c + 1)
        newPos = SourcePos name line newCol


bfTokenStream :: BS.ByteString -> BFTokenStream
bfTokenStream = BFTokenStream . L.map (fromJust . toInstr) . BS.unpack . minimize


bfInstructions :: BS.ByteString
bfInstructions = BSC8.pack "><+-.,[]"

-- | The function @minimize@ produces the smallest version of a Brainfuck source file
-- | by removed all the comments and other extraneous data.
minimize :: BS.ByteString -> BS.ByteString
minimize = BS.filter (`BS.elem` bfInstructions)
