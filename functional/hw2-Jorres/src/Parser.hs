module Parser where

import              Data.Maybe
import              Data.Char
import              Control.Applicative

data Parser s a  = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap f p = Parser (\lst -> fmap (\(a, lst) -> (f a, lst)) (runParser p lst))

instance Applicative (Parser s) where
    pure x = Parser (\lst -> Just (x, lst))
    (<*>) fp p = Parser (\lst -> case runParser fp lst of
        Nothing -> Nothing
        Just (f, rem) -> case runParser p rem of
            Nothing -> Nothing
            Just (arg, rem) -> Just ((f arg), rem))

instance Monad (Parser s) where
    return = pure
    (>>=) m f = Parser (\lst -> case runParser m lst of
        Nothing -> Nothing
        Just (res, rem) -> runParser (f res) rem)

instance Alternative (Parser s) where
    empty = Parser (\_ -> Nothing)
    (<|>) pl pr = Parser(\lst -> case runParser pl lst of
        Nothing -> runParser pr lst
        jst     -> jst)

ok :: Parser s ()
ok = Parser (\lst -> Just((), lst))

failAlways :: Parser s ()
failAlways = Parser (\_ -> Nothing)

eof :: Parser s ()
eof = Parser (\lst -> case lst of
    [] -> Just((), lst)
    _  -> Nothing)

wrapEof :: Parser s a -> Parser s ()
wrapEof p = p >> eof

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser (\lst -> case lst of
    []     -> Nothing
    (x:xs) -> if (p x) then Just (x, xs) else Nothing)

satisfy_ :: (s -> Bool) -> Parser s ()
satisfy_ p = (satisfy p) >> ok

element :: (Eq s) => s -> Parser s s
element c = satisfy (\test -> c == test)

element_ :: (Eq s) => s -> Parser s ()
element_ c = (element c) >> ok

stream :: (Eq s) => [s] -> Parser s [s]
stream [] = return []
stream (x:xs) = do
                c <- element x
                str <- stream xs
                return (c : str)

psp_ :: Parser Char ()
psp_ = Parser (\lst -> case lst of 
        [] -> Just((), [])
        nonempty -> runParser ((do
            element_ '('
            psp_
            element_ ')'
            psp_) <|> ok) lst)

psp :: Parser Char ()
psp = wrapEof psp_

takeWhile_ :: (Char -> Bool) -> Parser Char [Char]
takeWhile_ p = (do 
                c <- satisfy p
                str <- takeWhile_ p
                return (c : str)) <|> (return "")

number_ :: Parser Char Int
number_ = do 
        first <- satisfy isDigit 
        rem <- takeWhile_ isDigit
        return $ read $ first : rem

sign_ :: Parser Char Int
sign_ = do
        sign <- (element '+') <|> (element '-') <|> (return '+')
        return $ if sign == '+' then 1 else -1

number :: Parser Char Int
number = do
    mul <- sign_
    num <- number_
    return $ mul * num

skipWhitespaces :: Parser Char ()
skipWhitespaces = (do
    satisfy_ isSpace
    skipWhitespaces) <|> ok

parseComma :: Parser Char ()
parseComma = do 
    skipWhitespaces
    satisfy_ (== ',')
    skipWhitespaces

listParser :: Int -> Parser Char [Int]
listParser l = if l == 0 then (return []) else 
    (do
        n <- number
        if l > 1 then (do 
            parseComma
            rem <- listParser $ l - 1
            return $ n : rem)
        else return [n])

listlistParser :: Parser Char [[Int]]
listlistParser = (do
    l <- ((number) <|> return 0)
    if l == 0 then
        return []
    else (do 
            parseComma
            lst <- listParser l
            ((do 
                parseComma
                rem <- listlistParser
                return $ lst : rem) <|> return [lst])))
