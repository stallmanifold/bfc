module Brainfuck
    (
        BFProg(..),
        BFInstruction(..)
    )
where


data BFProg = BFProg [BFInstruction]

data BFInstruction = BFPointerInc
                   | BFPointerDec
                   | BFPointerDerefInc
                   | BFPointerDerefDec
                   | BFReadByte
                   | BFWriteByte
                   | BFBeginLoop
                   | BFEndLoop
                   | BFLoop [BFInstruction]

instance Show BFProg where
    show (BFProg instructions) = concatMap show instructions

instance Show BFInstruction where
    show BFPointerInc      = ">"
    show BFPointerDec      = "<"
    show BFPointerDerefInc = "+"
    show BFPointerDerefDec = "-"
    show BFReadByte        = ","
    show BFWriteByte       = "."
    show BFBeginLoop       = "["
    show BFEndLoop         = "]"

