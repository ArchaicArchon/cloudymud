--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE OverloadedStrings #-}
module Cloudy.Chat.Parser where

import qualified Data.ByteString.Char8 as BS
--import Data.ByteString.Char8
import Text.Parsec hiding (spaces)
--import Text.Parsec.ByteString 

data ChatCommand = 
	ChatChat BS.ByteString |
	ChatTell BS.ByteString BS.ByteString |
	ChatWho |
	ChatQuit |
	ChatUnclear BS.ByteString
	deriving (Show,Eq,Ord)


-- Usage: Test Parser String
test p = parse p "" . BS.unpack
--{- 
maybeSpaces = many space

spaces = many1 space

name = many1 (noneOf [' '])

message = many anyChar

chat = do
	maybeSpaces
	{-char 'c'
	char 'h'
	char 'a'
	char 't'
	-}
	string "chat"
	spaces
	msg <- message
	return (ChatChat . BS.pack $ msg)
---}

who = do
	maybeSpaces
	string "who"
	maybeSpaces
	return (ChatWho)


quit = do
	maybeSpaces
	string "quit"
	maybeSpaces
	return (ChatQuit)


--TODO: FIX: the tell parser does not work correctly
tell = do
	maybeSpaces
	string "tell"
	spaces
	nom <- name
	spaces
	msg <- message
	return (ChatTell (BS.pack nom) (BS.pack msg))

unclear = do
	msg <- message 
	return (ChatUnclear (BS.pack msg))


chatCommand = try chat <|> try tell <|> try who <|> try quit <|> unclear

parseChatCommand = test chatCommand

-- TODO: Add another parser that returns Unclear for fowrarding
--  to another node for parsing

-- TODO: Add a parser combinator parseChatCommand,
--   that combines the other parser combinators defined

wechat = test chat (BS.pack "  chat   mooose")

wedontchat = test chat (BS.pack "are we chatting now?")