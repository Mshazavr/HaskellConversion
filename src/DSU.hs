{-# LANGUAGE NamedFieldPuns #-}

module DSU (DSU(..), initDSU, valueDSU, sizeDSU, getPathValue) where


data DSU = Root {id :: String, size :: Integer} | Node {id :: String, parent :: DSU, value :: Float}

initDSU :: String -> DSU
initDSU id = Root {DSU.id=id, size=1}

_rootDSU :: DSU -> DSU
_rootDSU Root {DSU.id, size} = Root id size
_rootDSU Node {parent}       = _rootDSU parent

_reduceDSU :: DSU -> DSU
_reduceDSU Root {DSU.id, size} = Root id size
_reduceDSU Node { DSU.id, parent, value } =
  let newParent = _reduceDSU parent in
    case newParent of
      Root id parentSize -> Node { DSU.id=id, parent=newParent, value=value }
      Node { parent=root, value=parentValue } -> Node { DSU.id=id, parent=newParent, value=(value * parentValue) }

valueDSU :: DSU -> (DSU, Float)
valueDSU Root {DSU.id, size} = (Root id size, 1.0)
valueDSU dsu =
  let reducedDSU = _reduceDSU dsu in
    case reducedDSU of
      Node {} -> (reducedDSU, value reducedDSU)
      _ -> error "Impossible: non-root DSU turned into DSU during reduction!"

sizeDSU :: DSU -> Integer
sizeDSU Root {size}   = size
sizeDSU Node {parent} = sizeDSU parent

-- source = rate * target
getPathValue :: DSU -> DSU -> Maybe Float
getPathValue target source =
  let
    (newSource, sourceConversion) = valueDSU source
    (newTarget, targetConversion) = valueDSU target

  in
    case (newSource, newTarget) of
      (Root {}, Root {}) -> Nothing
      (Root {DSU.id=rootId}, Node {parent}) -> if DSU.id parent == rootId then Just targetConversion else Nothing
      (Node {parent}, Root {DSU.id=rootId}) -> if DSU.id parent == rootId then Just (1.0 / sourceConversion) else Nothing
      (Node {parent=sourceParent}, Node{parent=targetParent}) -> if DSU.id sourceParent == DSU.id targetParent then Just (targetConversion / sourceConversion) else Nothing







