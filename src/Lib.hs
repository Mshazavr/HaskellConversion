module Lib
  ( libFunc
  ) where

import           CharMap    (CharMap (..))
import           Data.Maybe (fromJust)
import qualified DSU        (DSU (..), getPathValue, initDSU, sizeDSU, valueDSU)
import qualified Trie       (Trie (..), addTrieString, getTrieString, initTrie)

eps :: Float
eps = 0.000001

addCurrency :: String -> Trie.Trie DSU.DSU -> Trie.Trie DSU.DSU
addCurrency s = Trie.addTrieString s $ DSU.initDSU s

-- source = rate * target
addConversion ::
     String -> String -> Float -> Trie.Trie DSU.DSU -> Trie.Trie DSU.DSU
addConversion target source rate tree =
  let (sourceDSU, sRate) =
        DSU.valueDSU $ fromJust $ Trie.getTrieString tree source
      (targetDSU, tRate) =
        DSU.valueDSU $ fromJust $ Trie.getTrieString tree target
      newRate = rate * (sRate / tRate)
      newSource =
        case sourceDSU of
          DSU.Root {} -> sourceDSU
          _           -> DSU.parent sourceDSU
      newTarget =
        case targetDSU of
          DSU.Root {} -> targetDSU
          _           -> DSU.parent targetDSU
   in case () of
        _
          | DSU.id newSource == DSU.id newTarget ->
            if abs (newRate - rate) < eps
              then tree
              else error "Conflicting conversion rates reported!"
        _
          | DSU.size newSource >= DSU.size newTarget ->
            ((Trie.addTrieString
                target
                (DSU.Node
                   { DSU.id = DSU.id newTarget
                   , DSU.parent = newSource
                   , DSU.value = newRate
                   })) .
             (Trie.addTrieString
                source
                (DSU.Root
                   { DSU.id = DSU.id newSource
                   , DSU.size = ((DSU.size newSource) + (DSU.size newTarget))
                   })))
              tree
        _ ->
          ((Trie.addTrieString
              source
              (DSU.Node
                 { DSU.id = DSU.id newSource
                 , DSU.parent = newTarget
                 , DSU.value = (1.0 / newRate)
                 })) .
           (Trie.addTrieString
              target
              (DSU.Root
                 { DSU.id = DSU.id newTarget
                 , DSU.size = ((DSU.size newSource) + (DSU.size newTarget))
                 })))
            tree

getConversion :: String -> String -> Trie.Trie DSU.DSU -> Float
getConversion target source tree =
  let
    targetDSU = Trie.getTrieString tree target
    sourceDSU = Trie.getTrieString tree source
  in case (targetDSU, sourceDSU) of
      (Nothing, _) -> error ("Target Currency doesn't exist: " ++ target)
      (_, Nothing)  -> error ("Source Currency doesn't exist: " ++ source)
      otherwise -> case DSU.getPathValue (fromJust targetDSU) (fromJust sourceDSU) of
        Nothing -> error ("The currencies are not connected: " ++ target ++ ", " ++ source)
        Just x -> x

libFunc :: IO ()
libFunc = do
  putStrLn "Hello, Haskell!"
  tree <- return $ Trie.initTrie
  tree <- return $ addCurrency "usd" tree
  tree <- return $ addCurrency "cad" tree
  tree <- return $ addCurrency "yen" tree
  tree <- return $ addCurrency "shmen" tree
  tree <- return $ addConversion "usd" "cad" 1.36 tree
  tree <- return $ addConversion "yen" "cad" 2.0 tree
  putStrLn $ show $ getConversion "usd" "cad" tree
  putStrLn $ show $ getConversion "usd" "usd" tree
  putStrLn $ show $ getConversion "cad" "usd" tree
  putStrLn $ show $ getConversion "cad" "yen" tree
  putStrLn $ show $ getConversion "yen" "usd" tree
  putStrLn $ show $ getConversion "yen" "shmen" tree

libFunc2 :: IO ()
libFunc2 = do
  putStrLn "Hello, Haskell!"
  tree <- return $ Trie.addTrieString "hello" 12 Trie.initTrie
  tree <- return $ Trie.addTrieString "world" 13 tree
  tree <- return $ Trie.addTrieString "helno" 21 tree
  putStrLn $ show $ Trie.getTrieString tree "hello"
  putStrLn $ show $ Trie.getTrieString tree "world"
  putStrLn $ show $ Trie.getTrieString tree "helno"
  putStrLn $ show $ Trie.getTrieString tree "bad string"
