module Goblin.Workshop.Result where

import Data.Either

type Err = String
type Ok = String
type Result = Either Err Ok

ok :: Ok -> Result
ok s = Right s

err :: Err -> Result
err s = Left s

isOk :: Result -> Bool
isOk = isRight

isErr :: Result -> Bool
isErr = isLeft
