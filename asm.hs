import Text.ParserCombinators.Parsec

{- System Record -}
type Register = Int
data Machine = Machine { ax :: Register
                       , bx :: Register
                       , cx :: Register
                       , dx :: Register
                       } deriving (Show)

-- Instruction
type InstName = String
type InstParam = String
data Inst = UnaryInst InstName InstParam
       | BinaryInst InstName InstParam InstParam
       deriving(Show)

{- INTERPRETATION -}
dispatchInstName :: [InstName -> Instruction]
dispatchInstName = [ ("mov", mov)
                   , ("add", add)
                   , ("sub", sub)
                   ]

dispatchInstParam :: [InstParam -> Location]
dispatchInstParam = [ ("ax", ax),
                      ("bx", bx)
                    ]
                      

type Command = (Machine -> Machine)
solidify :: Inst -> Command
solidify (UnaryInst iname iparam) = ($) <$> 
solidify (BinaryInst iname iparam iparam) = createCommand2 (dispatchInstName iname)
                                                           (dispatchInstParam iparam)
                                                           (dispatchInstParam iparam)
{- EXECUTION -}
mov :: Location -> Location -> Machine -> Machine
mov a b m = m { b = a m }

{- PARSING 
 -###############################################################
 - -}
asmFile :: GenParser Char st [Inst]
asmFile = do result <- many line
             eof
             return result

--asmFile = endBy line eol

line :: GenParser Char st Inst
line = try binaryLine <|> unaryLine

unaryLine = do inst <- many1 symbol
               spaces'
               param <- many1 symbol
               spaces
               return (UnaryInst inst param)

binaryLine = do inst <- many1 symbol
                spaces'
                param1 <- many1 symbol
                spaces'
                char ','
                spaces'
                param2 <- many1 symbol
                spaces
                return (BinaryInst inst param1 param2)

spaces' = many (char ' ')
symbol = noneOf ", \n\r"
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseASM :: String -> Either ParseError [Inst]
parseASM input = parse asmFile "(unknown)" input
