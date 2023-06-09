module CharMap (
  CharMap,
  charMapGet,
  charMapUpdate,
  charMapEmpty
) where

data CharMap tp = CharMap {
  a :: Maybe tp,
  b :: Maybe tp,
  c :: Maybe tp,
  d :: Maybe tp,
  e :: Maybe tp,
  f :: Maybe tp,
  g :: Maybe tp,
  h :: Maybe tp,
  i :: Maybe tp,
  j :: Maybe tp,
  k :: Maybe tp,
  l :: Maybe tp,
  m :: Maybe tp,
  n :: Maybe tp,
  o :: Maybe tp,
  p :: Maybe tp,
  q :: Maybe tp,
  r :: Maybe tp,
  s :: Maybe tp,
  t :: Maybe tp,
  u :: Maybe tp,
  v :: Maybe tp,
  w :: Maybe tp,
  x :: Maybe tp,
  y :: Maybe tp,
  z :: Maybe tp
}

charMapGet :: CharMap tp -> Char -> Maybe tp
charMapGet m 'a'  = a m
charMapGet m 'b'  = b m
charMapGet m 'c'  = c m
charMapGet m 'd'  = d m
charMapGet m 'e'  = e m
charMapGet m 'f'  = f m
charMapGet m 'g'  = g m
charMapGet m 'h'  = h m
charMapGet m 'i'  = i m
charMapGet m 'j'  = j m
charMapGet m 'k'  = k m
charMapGet m 'l'  = l m
charMapGet _m 'm' = m _m
charMapGet m 'n'  = n m
charMapGet m 'o'  = o m
charMapGet m 'p'  = p m
charMapGet m 'q'  = q m
charMapGet m 'r'  = r m
charMapGet m 's'  = s m
charMapGet m 't'  = t m
charMapGet m 'u'  = u m
charMapGet m 'v'  = v m
charMapGet m 'w'  = w m
charMapGet m 'x'  = x m
charMapGet m 'y'  = y m
charMapGet m 'z'  = z m

charMapUpdate :: CharMap tp -> Char ->  tp -> CharMap tp
charMapUpdate m 'a' v = m { a = (Just v) }
charMapUpdate m 'b' v = m { b = (Just v) }
charMapUpdate m 'c' v = m { c = (Just v) }
charMapUpdate m 'd' v = m { d = (Just v) }
charMapUpdate m 'e' v = m { e = (Just v) }
charMapUpdate m 'f' v = m { f = (Just v) }
charMapUpdate m 'g' v = m { g = (Just v) }
charMapUpdate m 'h' v = m { h = (Just v) }
charMapUpdate m 'i' v = m { i = (Just v) }
charMapUpdate m 'j' v = m { j = (Just v) }
charMapUpdate m 'k' v = m { k = (Just v) }
charMapUpdate m 'l' v = m { l = (Just v) }
charMapUpdate m 'm' v = m { m = (Just v) }
charMapUpdate m 'n' v = m { n = (Just v) }
charMapUpdate m 'o' v = m { o = (Just v) }
charMapUpdate m 'p' v = m { p = (Just v) }
charMapUpdate m 'q' v = m { q = (Just v) }
charMapUpdate m 'r' v = m { r = (Just v) }
charMapUpdate m 's' v = m { s = (Just v) }
charMapUpdate m 't' v = m { t = (Just v) }
charMapUpdate m 'u' v = m { u = (Just v) }
charMapUpdate m 'v' v = m { v = (Just v) }
charMapUpdate m 'w' v = m { w = (Just v) }
charMapUpdate m 'x' v = m { x = (Just v) }
charMapUpdate m 'y' v = m { y = (Just v) }
charMapUpdate m 'z' v = m { z = (Just v) }


charMapEmpty = CharMap {
  a = Nothing,
  b = Nothing,
  c = Nothing,
  d = Nothing,
  e = Nothing,
  f = Nothing,
  g = Nothing,
  h = Nothing,
  i = Nothing,
  j = Nothing,
  k = Nothing,
  l = Nothing,
  m = Nothing,
  n = Nothing,
  o = Nothing,
  p = Nothing,
  q = Nothing,
  r = Nothing,
  s = Nothing,
  t = Nothing,
  u = Nothing,
  v = Nothing,
  w = Nothing,
  x = Nothing,
  y = Nothing,
  z = Nothing
}
