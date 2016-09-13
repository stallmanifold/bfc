module Brainfuck.Parser
    (
        
    ) where

import Text.Megaparsec.Prim
import Brainfuck.Lexer
import Brainfuck.Instructions
import Control.Monad
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


bfAtomicInstr :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m BFInstruction
bfAtomicInstr maybeTok = bfPointerInc maybeTok 
                      <|> bfPointerDec maybeTok
                      <|> bfPointerDerefInc maybeTok
                      <|> bfPointerDerefDec maybeTok
                      <|> bfReadByte maybeTok
                      <|> bfWriteByte maybeTok

bfAtomicInstrList :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m [BFInstruction]
bfAtomicInstrList maybeTok = return <$> bfAtomicInstr maybeTok

bfBeginLoop :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m BFInstruction
bfBeginLoop = token (atomicInstrTest BFBeginLoop BFRightBrace) 


bfEndLoop :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m BFInstruction
bfEndLoop = token (atomicInstrTest BFEndLoop BFLeftBrace)


bfLoop :: ErrorComponent e => Maybe (BFToken) -> ParsecT e BFTokenStream m [BFInstruction]
bfLoop maybeTok = do
    begin <- bfBeginLoop maybeTok
    body  <- many ((bfAtomicInstrList maybeTok) <|> bfLoop maybeTok)
    end   <- bfEndLoop maybeTok
    return $ [begin] ++ join body ++ [end]