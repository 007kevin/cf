{-# LANGUAGE OverloadedStrings #-}
module Submit (initSubmit) where

import Control.Monad.Trans.Except (ExceptT,
                                   runExceptT)

initSubmit = "dummy"
-- initSubmit :: String -> IO()
-- initSubmit file = do
--   result <- runExceptT 4 attemptSubmit file
