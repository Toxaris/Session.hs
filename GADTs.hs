{-# LANGUAGE GADTs #-}
module GADTs where

data Process a b where
  Send :: Show a => a -> Process b c -> Process (a, b) (a -> c)
  Receive :: Show a => (a -> Process b c) -> Process (a -> b) (a, c)
  Done :: Process () ()

run :: Process a b -> Process b a -> IO ()
run (Send x y) (Receive f) = do
  putStr "--> "
  print x
  run y (f x)
run (Receive f) (Send x y) = do
  putStr "<-- "
  print x
  run (f x) y
run Done Done = do
  putStr "done"

-- echo server
echo = Receive (\x -> Send x Done)

-- good client
good = Send 5 (Receive (\f -> Done))

-- bad client
bad = Send 5 Done

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
