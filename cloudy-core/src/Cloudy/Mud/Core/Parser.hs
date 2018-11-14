--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE OverloadedStrings #-}
module Cloudy.Mud.Core.Parser where

import qualified Data.ByteString.Char8 as BS
--import Data.ByteString.Char8
import Text.Parsec hiding (spaces)
--import Text.Parsec.ByteString 

data CoreCommand = 
	CommandLook |
	CommandGo BS.ByteString |
	CommandUnclear BS.ByteString
	deriving (Show,Eq,Ord)

{- TODO: BUG FIX
my parsers will parse the current word in this module and Chat.Parser,
but will match additional characters, I need to fix that,
but how?
-}

-- Usage: Test Parser String
test p = parse p "" . BS.unpack
--{- 
maybeSpaces = many space

--spaces = many1 space

word = many1 (noneOf [' '])

--newLine = char '\n'

message = many anyChar

parseLook = do
	maybeSpaces
	string "look"
	maybeSpaces
	eof
	return CommandLook

---}

parseGo = do
	maybeSpaces
	string "go"
	maybeSpaces
	direction <- word
	maybeSpaces
	eof
	return (CommandGo (BS.pack direction))

parseUnclear = do
	msg <- message
	eof 
	return (CommandUnclear (BS.pack msg))

coreCommand 
	= try parseLook 
	<|> try parseGo 
	<|> parseUnclear

parseCoreCommand ::  BS.ByteString -> Either ParseError CoreCommand
parseCoreCommand string = test coreCommand string
