{-# LANGUAGE NamedFieldPuns #-}

module Trie (Trie, addTrieString, getTrieString, initTrie) where

import           CharMap (CharMap (..), charMapEmpty, charMapGet, charMapUpdate)

data Trie a = Node {
  edges :: (CharMap (Trie a)),
  value :: (Maybe a)
}

initTrie :: Trie a
initTrie = Node {edges=charMapEmpty, value=Nothing}

addTrieString :: [Char] -> a -> Trie a -> Trie a
addTrieString [] v (Node {edges}) = (Node {edges=edges, value= Just v})
addTrieString (c:cs) v (Node {edges, value}) =
  case (charMapGet edges c) of
    Nothing -> Node {
      edges=(charMapUpdate edges c (addTrieString cs v initTrie)),
      value=value
    }
    Just t  -> Node {
      edges=(charMapUpdate edges c (addTrieString cs v t)),
      value=value
    }

getTrieString :: Trie a -> [Char] -> Maybe a
getTrieString (Node {value}) [] = value
getTrieString (Node {edges}) (c:cs) =
  case (charMapGet edges c) of
    Nothing -> Nothing
    Just t  -> getTrieString t cs

