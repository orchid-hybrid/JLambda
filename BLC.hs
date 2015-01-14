module BLC (
 ) where

import Control.Applicative hiding ((<|>))
import Data.List
import Text.Parsec

import CCC(Expr(..))

-- binary lambda terms

runParseTerm = runParser (parseTerm <* eof) () ""

parseTerm :: Parsec String () Expr
parseTerm = try parseVariable <|> try parseLambda <|> parseApplication

parseVariable = do
    n <- many1 (char '1')
    char '0'
    return (Var ((length n) - 1))

parseLambda = do
    string "00"
    t <- parseTerm
    return (Abs t)

parseApplication = do
    string "01"
    p <- parseTerm
    q <- parseTerm
    return (App p q)
