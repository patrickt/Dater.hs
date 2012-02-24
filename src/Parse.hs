{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Parse
    ( Parser
    , Parse(parse)
    , colon
    , dot
    , slash
    , whitespace
    , whited
    , liftReadS
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe (listToMaybe)
import Data.Ratio ((%))
import Text.ParserCombinators.Parsec hiding (Parser, parse, many)

type Parser = GenParser Char ()
class Parse a where parse :: Parser a
instance Parse Int where parse = read <$> many1 digit
instance Parse Integer where parse = read <$> many1 digit
instance Parse Rational where parse = (%) <$> (parse <* slash) <*> parse
instance Parse a => Parse (Maybe a) where parse = optionMaybe parse

colon, dot, slash, whitespace :: Parser ()
colon      = void $ char ':'
dot        = void $ char '.'
slash      = void $ char '\\'
whitespace = void $ oneOf " \t"

whited :: Parser a -> Parser a
whited x = w *> x <* w where w = many whitespace

liftReadS :: ReadS a -> String -> Parser a
liftReadS f = maybe (unexpected "no parse") (pure . fst) .
              listToMaybe . filter (null . snd) . f
