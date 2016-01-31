addOneIfOdd n = case odd n of
  True -> foo n
  False -> n
  where foo = (\n -> n + 1)
