import Text.ParserCombinators.Parsec

-- Instruction
type InstName = String
type InstParam = String
data Inst = UnaryInst InstName InstParam
       | BinaryInst InstName InstParam InstParam
       deriving(Show)

asmFile :: GenParser Char st [Inst]
asmFile = do result <- many line
          eof
          return result

--asmFile = endBy line eol

line :: GenParser Char st Inst
line = try binaryLine <|> unaryLine
 
unaryLine = do inst <- many1 symbol
            many (char ' ')
            param <- many1 symbol
            return (UnaryInst inst param)
 
binaryLine = do inst <- many1 symbol
             many (char ' ')
             param1 <- many1 symbol
             many (char ' ')
             char ','
             many (char ' ')
             param2 <- many1 symbol
             return (BinaryInst inst param1 param2)

symbol = noneOf ", \n\r"
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"