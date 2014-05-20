import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Monad.State
import qualified Data.Map as M


data Expression = Constant Double
        | Identifier String
        | Addition Expression Expression
        | Subtraction Expression Expression
        | Multiplication Expression Expression
        | Division Expression Expression
        | Modulus Expression Expression
        | Negation Expression
        deriving (Show)

data Statement = PrintStatement Expression
        deriving (Show)

lexer :: TokenParser()
lexer =  makeTokenParser(javaStyle{
        opStart  = oneOf "+-*/%",
        opLetter = oneOf "+-*/%"
    })

parseNumber :: Parser Expression
parseNumber = do
    val <- naturalOrFloat lexer
    case val of 
        Left i -> return $ Constant $ fromIntegral i
        Right n -> return $ Constant $ n

parseExpression :: Parser Expression
parseExpression = (flip buildExpressionParser) parseTerm $ [
        [ Prefix (reservedOp lexer "-" >> return Negation),
          Prefix (reservedOp lexer "+" >> return id) ],
        [ Infix  (reservedOp lexer "*" >> return Multiplication) AssocLeft,
          Infix  (reservedOp lexer "/" >> return Division) AssocLeft,
          Infix  (reservedOp lexer "%" >> return Modulus)AssocLeft],
        [ Infix  (reservedOp lexer "+" >> return Addition) AssocLeft,
          Infix  (reservedOp lexer "-" >> return Subtraction) AssocLeft]
    ]

parseTerm :: Parser Expression
parseTerm = parens lexer parseExpression <|> parseNumber <|> (identifier lexer >>= return Identifier)

parsePrint :: Parser Statement
parsePrint = do
    reserved lexer "print"
    n <-  parseExpression
    return $ PrintStatement n

parseInput :: Parser Statement
parseInput = do
    whiteSpace lexer
    s <- parsePrint
    eof
    return s

type Calculator a = StateT(M.Map String Expression) IO a

interpretExpression :: Expression -> Calculator Double
interpretExpression (Constant n) =  return n
interpretExpression (Multiplication e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    return (v1*v2)
interpretExpression (Division e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    return (v1/v2)
interpretExpression (Modulus e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    let n1 = floor v1
        n2 = floor v2        
    return (fromInteger (n1 `mod` n2))
interpretExpression (Addition e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    return (v1+v2)
interpretExpression (Subtraction e1 e2) = do
    v1 <- interpretExpression e1
    v2 <- interpretExpression e2
    return (v1-v2)
interpretExpression (Negation e1) = do
    v1 <- interpretExpression e1    
    return (negate v1)

interpretStatement :: Statement -> Calculator ()
interpretStatement (PrintStatement expr) = do
    n <- interpretExpression expr
    liftIO $ print n



calculate :: String -> IO()
calculate s = do
    case ret of
        Left e -> putStr $ "error: " ++ (show e)
        Right n -> evalStateT (interpretStatement n) M.empty
    where
        ret = parse parseInput "" s

main :: IO()
main = getContents >>=(mapM_ calculate) .lines
