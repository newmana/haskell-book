listPure = pure :: (a -> [] a)
listAp = (<*>) :: [] (a -> b) -> [] a -> [] b
listTest = listAp (listPure (+1)) (listPure 2)

ioPure = pure :: a -> IO a
ioAp = (<*>) :: IO (a -> b) -> IO a -> IO b
ioTest = ioAp (ioPure (+1)) (ioPure 2)

tuplePure :: Monoid b => a -> (b, a)
tuplePure = pure :: Monoid b => a -> ((,) b) a

tupleAp :: Monoid c => (c, a -> b) -> (c, a) -> (c, b)
tupleAp = (<*>) :: Monoid c => (,) c (a -> b) -> (,) c a -> (,) c b

tupleTest = tupleAp (tuplePure (+1) :: ([Int], Int -> Int)) (tuplePure 2 :: ([Int], Int))
tupleTest2 = tupleAp (tuplePure (+1) :: (String, Int -> Int)) (tuplePure 2 :: (String, Int))

arrowPure = pure :: (a -> ((->) e) a)
arrowAp = (<*>) :: ((->) e) (a -> b) -> ((->) e) a -> ((->) e) b
arrowTest = arrowAp (arrowPure (+1)) (arrowPure 2) id

