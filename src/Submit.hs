{-# LANGUAGE OverloadedStrings #-}
module Submit (initSubmit) where

import AppError
import Control.Monad.Trans.Except (ExceptT,
                                   runExceptT)
initSubmit :: String -> Maybe String -> Maybe String -> IO ()
initSubmit file Nothing Nothing = putStrLn file


-- attemptSubmit (file, lang, prob)
-- initSubmit :: String -> IO()
-- initSubmit file = do
--   result <- runExceptT 4 attemptSubmit file
