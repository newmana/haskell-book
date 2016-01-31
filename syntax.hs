x = (+)
f xs = w `x` 1
  where w = length xs

-- (\x -> x) "Asdf"
id x = x

-- (\(x : xs) -> x) "asdf"
first = \(x : xs) -> x
