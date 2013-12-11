{-# LANGUAGE TypeFamilies #-}
module TypeFamilies where

class Session a where
  type Dual a
  run :: a -> Dual a -> IO ()

instance (Show a, Session b) => Session (a -> b) where
  type Dual (a -> b) = (a, Dual b)
  run f (x, y) = do
    putStr "<-- "
    print x
    run (f x) y

instance (Show a, Session b) => Session (a, b) where
  type Dual (a, b) = a -> Dual b
  run (x, y) f = do
    putStr "--> "
    print x
    run y (f x)

instance Session () where
  type Dual () = ()
  run () () = do
    putStr "done"

-- echo server
echo x = (x, ())

-- good client
good = (5, \5 -> ())

-- bad client
bad = (5, ())

main = do
  -- accepted
  putStrLn "run echo good"
  run echo good
  putStrLn ""

  putStrLn "run good echo"
  run good echo
  putStrLn ""

  -- rejected
  {-
  putStrLn "run echo bad"
  run echo bad
  putStrLn ""

  putStrLn "run bad echo"
  run bad echo
  putStrLn ""
  -}
