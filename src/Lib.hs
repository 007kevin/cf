module Lib
    ( start
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

start :: IO ()
start = putStrLn "Hello Kevin!"
