module Parser.Impl where

import SubsAst

-- more includes below
import Control.Monad

import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
import Text.Parsec.Prim hiding (token)


-- from Lecturer Ken
munch :: (Char -> Bool) -> Parser String
munch temp = do
            result <- many $ satisfy temp
            notFollowedBy $ satisfy temp
            return result


-- Function takes token and then parses it whitespace function
token :: Parser a -> Parser a
token temp = do
            x <- temp
            sToken ""
            return x

-- Copied from slide 14 of second parser lecture
whitespace :: Parser a -> Parser a
whitespace p = do
                  res <- p
                  spaces
                  return res


sToken :: String -> Parser String
sToken s = try (spaces >> string s <* spaces)

-- function whichs parses key word
keyWordParser :: String -> Parser ()
keyWordParser s = token $ do
                        _ <- string s
                        notFollowedBy alphaNum

keywords :: [String]
keywords = ["undefined", "false", "true", "of", "if","for"]

parseString :: String -> Either ParseError Expr
parseString sTemp =
                case parse parseToEndOfFile "Parsing Error" sTemp of
                    Right a -> Right a
                    Left e -> Left e

parseToEndOfFile :: Parser Expr
parseToEndOfFile = do
                      -- taking care of whitespaces at the start
                      whitespaceParser
                      result <- parseExpr
                      -- checker for end of file after parsing went okay
                      eof
                      return result

-- main parser function
parseExpr :: Parser Expr
parseExpr = do
            expr <- parseExpr1
            parseExpr' expr


-- chaining parsing functions
parseExpr' :: Expr -> Parser Expr
parseExpr' expr' = do
                void (sToken ",")
                expr <- parseExpr
                return (Comma expr' expr)
            <|> -- expr' is eps
                return expr'

parseExpr1 :: Parser Expr -- parse Ident =
parseExpr1 =
            try ( do
                    ident <- parseIdent
                    void  (sToken "=")
                    e <- parseExpr1
                    return  (Assign ident e) )
            <|>
                parseExpr2

apAddOp :: Parser (Expr -> Expr -> Expr)
apAddOp = (do
              _ <- sToken "<"
              return (\x y -> Call "<" [x, y]))
              <|> (do
                      _ <- sToken "==="
                      return (\x y -> Call "===" [x, y]))

-- chaining
parseExpr2 :: Parser Expr
parseExpr2 = parseExpr3 `chainl1` apAddOp

parseExpr3 :: Parser Expr
parseExpr3 = do
            expr1 <- parseExpr4
            parseExpr3' expr1

parseExpr3' :: Expr -> Parser Expr
parseExpr3' expr1 = do
                op <- sToken "+" <|> sToken "-"
                expr2 <- parseExpr4
                parseExpr3' (Call op [expr1, expr2])
             <|>
                return expr1


parseExpr4 :: Parser Expr
parseExpr4 = do
            expr1 <- parseExprTerm
            parseExpr4' expr1

parseExpr4' :: Expr -> Parser Expr
parseExpr4' expr1 = do
                      op <- sToken "*" <|> sToken "%"
                      expr2 <- parseExprTerm
                      parseExpr4' (Call op [expr1, expr2])
                    <|>
                       return expr1

parseExprTerm :: Parser Expr
parseExprTerm = parseNumber
        <|> parseForString
        <|> try parseTrue
        <|> try parseFalse
        <|> try parseUndefined
        <|> parseParentheses
        <|> parseExpressIdent
        <|> parseExpressArray

-- function for parsing numbers
parseNumber :: Parser Expr
parseNumber = (try parseNumberNeg <|> parseNumberPos) <* spaces

-- taking care of positive numbers
parseNumberPos :: Parser Expr
parseNumberPos =  do
  ns <- many1 digit
  if length ns < 9 -- check if number is less than 9 digits long
    then return (Number (read ns))
    else fail "Number too long"

-- checking if number is negative
parseNumberNeg :: Parser Expr
parseNumberNeg = do
  x <- string "-"
  ns <- many1 digit
  if length ns < 9 -- check if number is less than 9 digits long
    then return (Number (read (x ++ ns)))
    else fail "Number too long"

-- parsing string
parseForString :: Parser Expr
parseForString = token $ do
                    _ <- string "'"
                    s <- parseForStringR ""
                    return (String s)

-- recursion for string
parseForStringR :: String -> Parser String
parseForStringR string1 = do
                            string2 <- munch breaker
                            break1 <- anyToken
                            case break1 of
                              '\'' -> return $ string1 ++ string2
                              '\\' -> do
                                        string3 <- backslashCheck
                                        parseForStringR $ string1 ++ string2
                                                          ++ string3
                              temp -> fail $ "Unrecognized" ++
                                              " char: \"" ++ [temp] ++ "\""


-- Parser for "true"
parseTrue :: Parser Expr
parseTrue = whitespace $ do
                            _ <- string "true"
                            notFollowedBy alphaNum
                            return TrueConst

-- Parser for "false"
parseFalse :: Parser Expr
parseFalse = whitespace $ do
                            _ <- string "false"
                            notFollowedBy alphaNum
                            return FalseConst

-- Parser for "undefined"
parseUndefined :: Parser Expr
parseUndefined = whitespace $ do
                                _ <- string "undefined"
                                return Undefined

-- Function which takes care of parantheses
parseParentheses :: Parser Expr
parseParentheses = do
                 void $ sToken "("
                 e <- parseExpr
                 void $ sToken ")"
                 return e

-- Parser for Ident expression
parseExpressIdent :: Parser Expr
parseExpressIdent = do
                      ident <- parseIdent
                      parseExpressIdent' ident

parseExpressIdent' :: Ident -> Parser Expr
parseExpressIdent' i = do
                    void (sToken "(")
                    exprs <- parseExprs
                    void (sToken ")")
                    return (Call i exprs)
                <|>
                    return (Var i)

parseExprs :: Parser [Expr]
parseExprs = do
            x <- parseExpr1
            xs <- parseExprs'
            return (x:xs)
         <|> -- if first alternative fails, return empty array
            return []

parseExprs' :: Parser [Expr]
parseExprs' = do
            void (sToken ",")
            x <- parseExpr1
            xs <- parseExprs'
            return (x:xs)
         <|> -- if first alternative fails, return empty array
            return []

-- Parser for array expression
parseExpressArray :: Parser Expr
parseExpressArray = do
                void (sToken "[")
                parseExpressArray'

parseExpressArray' :: Parser Expr
parseExpressArray' = try (do
                    exprs <- parseExprs
                    void(sToken "]")
                    return (Array exprs))
              <|>
              do
                arrayFor <- pArrayFor
                void(sToken "]")
                return (Compr arrayFor)

-- Parser for simple array comprehenssion
pArrayCompr :: Parser ArrayCompr
pArrayCompr =   try pArrayFor
              <|>
                try pArrayIf
              <|>
                do
                e <- parseExpr1
                return (ACBody e)

-- Parser for array comprehenssions "if"
pArrayIf :: Parser ArrayCompr
pArrayIf = do
               keyWordParser "if"
               _ <- sToken "("
               expr <- parseExpr1
               _ <- sToken ")"
               ac <- pArrayCompr
               return (ACIf expr ac)

-- Parser for array comprehenssions "for"
pArrayFor :: Parser ArrayCompr
pArrayFor = do
              keyWordParser "for"
              _ <- sToken "("
              ident <- parseIdent
              keyWordParser "of"
              expr <- parseExpr1
              _ <- sToken ")"
              ac <- pArrayCompr
              return (ACFor ident expr ac)

-- Function to check if string has invalid char withing itself
breaker :: Char -> Bool
breaker '\'' = False
breaker '\\' = False
breaker    c = isPrint c

-- Parser for ident
parseIdent :: Parser String
parseIdent =  do
                x <- letter
                xs <- many (alphaNum <|> satisfy ('_' ==))
                spaces
                if (x:xs) `elem` keywords
                  then fail ((x:xs) ++ " is a keyword")
                  else return (x:xs)

-- called after a backslash has ben read
backslashCheck :: Parser String
backslashCheck = do
                    checkfail <- anyToken
                    case checkfail of
                        'n' -> return "\n"
                        '\'' -> return "'"
                        't' -> return "\t"
                        '\\' -> return "\\"
                        '\n' -> do
                                   whitespaceParser
                                   return ""
                        _ -> fail "Unrecognized backslash withing string"

-- Function to parse whitespace
whitespaceParser :: Parser ()
whitespaceParser = do
                    first <- many $ oneOf " \n\t"
                    second <- many commentHandler
                    when (not (null first) || not (null second)) $
                      do whitespaceParser -- recursive call
                         return ()

-- Comment handling
commentHandler :: Parser String
commentHandler = try(do
                    _ <- string "//"
                    manyTill anyChar newline)
                    <|>
                      do
                        _ <- string "//"
                        -- deliberately not used symbol to allow empty comments
                        manyTill anyChar eof
