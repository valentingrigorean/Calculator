module Calc(calcUI) where
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Monad.State
import qualified Data.Map as M

data Expr = Constant Double
	| Identifier String
	| Multiplication Expr Expr
	| Division Expr Expr
	| Modulus Expr Expr
	| Addition Expr Expr
	| Subtraction Expr Expr
	| Negation Expr
	| DoubleNegation Expr
	| Power Expr Expr
	| Sqrt Expr
	| Sin Expr
	| Cos Expr
	deriving(Show)
	
data Stmt = PrintStmt Expr	
	| AssignStmt String Expr	
	deriving(Show)
	
defaultValues :: LanguageDef st
defaultValues = LanguageDef 
	{	commentStart 	= "/*"
	,	commentEnd		= "*/"
	,	commentLine 	= "//"
	,	nestedComments  = True
	,	identStart 		= letter <|> char '_'
	,	identLetter 	= alphaNum <|> oneOf "_'"
	,	opStart 		= oneOf "+-&/%^"
	,	opLetter 		= oneOf "+-&/%^"
	,	reservedNames 	= ["cos","sin","sqrt"]
	,	reservedOpNames = ["="]
	,	caseSensitive 	= False
	}
	
table = 
	[	
		[	Prefix 	(reservedOp lexer "sqrt" >> return Sqrt),
			Prefix 	(reservedOp lexer "sin"	 >> return Sin),
			Prefix	(reservedOp lexer "cos"  >> return Cos),
			Prefix 	(reservedOp lexer "--" 	 >> return DoubleNegation),			
			Prefix 	(reservedOp lexer "-"  	 >> return Negation),
			Prefix 	(reservedOp lexer "+"  	 >> return id)			
		]
	,	[	Infix	(reservedOp lexer "*"    >> return Multiplication) AssocLeft,
			Infix	(reservedOp lexer "/"    >> return Division) AssocLeft,
			Infix	(reservedOp lexer "%"    >> return Modulus) AssocLeft,
			Infix	(reservedOp lexer "^"    >> return Power) AssocRight
		]
	,	[	Infix	(reservedOp lexer "+"    >> return Addition) AssocLeft,
			Infix	(reservedOp lexer "-"    >> return Subtraction) AssocLeft
		]
	]

lexer :: TokenParser()
lexer = makeTokenParser(defaultValues)

--parse
parseExpr :: Parser Expr
parseExpr = buildExpressionParser table parseTerm 
	
parseTerm :: Parser Expr
parseTerm = 
	parens lexer parseExpr
	<|> parseNumber
	<|> parseIdentifier
	
parsePrint :: Parser Stmt
parsePrint = do	
	expr <- parseExpr
	return (PrintStmt expr)
	
parseInput :: Parser Stmt
parseInput = do	
	whiteSpace lexer
	s <- (try parseAssign <|> parsePrint)	
	return s

parseAssign :: Parser Stmt
parseAssign = do		
	ident <- identifier lexer
	reservedOp lexer "="
	expr <- parseExpr
	return (AssignStmt ident expr)
	
parseNumber :: Parser Expr
parseNumber = do
	val <- naturalOrFloat lexer
	case val of
		Left i ->  return (Constant (fromIntegral i))
		Right n -> return (Constant n)
parseIdentifier :: Parser Expr
parseIdentifier = do
	ident <- identifier lexer
	return $ Identifier ident
	
--interpret expr
type Calculator a = StateT (M.Map String Expr) IO a

interpretExpr :: Expr -> Calculator Double

interpretExpr (Constant n) = return (n)

interpretExpr (Identifier i) = do
	vmap <- get
	case M.lookup i vmap of
		Nothing -> fail ("Unknown identifier: " ++ i)
		Just e -> interpretExpr e
	
interpretExpr (Multiplication e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2
	return (v1*v2)
	
interpretExpr (Division e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2
	case v2 of
		0 -> fail ("Division by 0")
		_ -> return (v1/v2)
	
interpretExpr (Modulus e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2	
	return (fromIntegral ((floor v1) `mod` (floor v2)))
	
interpretExpr (Addition e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2
	return (v1+v2)
	
interpretExpr (Subtraction e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2
	return (v1-v2)
	
interpretExpr (Negation e1) = do
	v1 <- interpretExpr e1
	return (negate v1)
	
interpretExpr (DoubleNegation e1) = do
	v1 <- interpretExpr e1
	return v1

interpretExpr (Sqrt e1) = do
	v1 <- interpretExpr e1	
	return (sqrt v1)
	
interpretExpr (Power e1 e2) = do
	v1 <- interpretExpr e1
	v2 <- interpretExpr e2
	return (v1 ^ (floor v2))
	
interpretExpr (Sin e1) = do
	v1 <- interpretExpr e1
	return (sin v1)
	
interpretExpr (Cos e1) = do
	v1 <- interpretExpr e1
	return (cos v1)	
	
--interpret Stmt
interpretStmt :: Stmt ->Calculator()
interpretStmt (PrintStmt expr) = do	
	n <- interpretExpr expr
	liftIO $ print n
	
interpretStmt (AssignStmt ident expr) = do
	n <- interpretExpr expr
	modify (M.insert ident (Constant n))

inter :: Stmt -> Calculator Double
inter (PrintStmt expr) = do
	n <- interpretExpr expr
	return n	

calculateUI s = 
	case ret of 
		Left e -> fail ("Invalid Expr")
		Right n -> evalStateT (inter n) M.empty
	where
		ret = parse parseInput "" s

calcUI s = do
	val <- calculateUI s
	return (show val)

calculate :: String -> Calculator ()
calculate s = 
	case ret of
		Left e ->  fail ("Invalid Expr")
		Right n -> interpretStmt n
	where
		ret = parse parseInput "" s		
calculator ::String -> Calculator()
calculator s = 
	mapM_ calculate (lines s)

main :: IO()
main = do 
	cont <- readFile "input.txt"
	evalStateT (calculator cont) M.empty

