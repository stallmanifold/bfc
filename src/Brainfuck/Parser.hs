module Brainfuck.Parser
    (
        atomicInstrTest
    ) where

import Text.Megaparsec.Prim
import Brainfuck.Lexer
import Brainfuck.Instructions
import Data.List.NonEmpty
import Data.Set                as Set
import Text.Megaparsec



atomicInstrTest :: BFInstruction -> BFToken
                                 -> BFToken 
                                 -> Either (Set (ErrorItem (BFToken)), Set (ErrorItem (BFToken)), Set e) BFInstruction

atomicInstrTest desiredInstr desiredToken currentToken =
    if currentToken == desiredToken
        then Right desiredInstr
        else Left (Set.singleton (Tokens (currentToken :| [])), Set.empty, Set.empty)


bfPointerInc :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m BFInstruction
bfPointerInc = token (atomicInstrTest BFPointerInc BFRightBracket)

bfPointerDec :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m BFInstruction
bfPointerDec = token (atomicInstrTest BFPointerDec BFLeftBracket)

bfPointerDerefInc :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m BFInstruction
bfPointerDerefInc = token (atomicInstrTest BFPointerDerefInc BFPlusSign)

bfPointerDerefDec :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m BFInstruction
bfPointerDerefDec = token (atomicInstrTest BFPointerDerefDec BFMinusSign)

bfReadByte :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m BFInstruction
bfReadByte = token (atomicInstrTest BFReadByte BFDot)

bfWriteByte :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m BFInstruction
bfWriteByte = token (atomicInstrTest BFWriteByte BFComma)

--bfBeginLoop
--bfEndLoop