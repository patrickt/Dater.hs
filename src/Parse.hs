{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Parse
    ( Parser
    , Parse(parse)
    , colon
    , dot
    , slash
    , whitespace
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import Text.ParserCombinators.Parsec hiding (Parser, parse, many, optional)

type Parser = GenParser Char ()

class Parse a where parse :: Parser a

instance Parse Int where parse = read <$> many1 digit
instance Parse Integer where parse = read <$> many1 digit
instance Parse Rational where parse = (%) <$> (parse <* slash) <*> parse
instance Parse a => Parse (Maybe a) where parse = optional parse

colon, dot, slash, whitespace :: Parser ()
colon      = void $ char ':'
dot        = void $ char '.'
slash      = void $ char '\\'
whitespace = void $ oneOf " \t"