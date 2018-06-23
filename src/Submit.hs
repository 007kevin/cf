{-# LANGUAGE OverloadedStrings #-}
module Submit (initSubmit) where

import AppError
import System.FilePath.Posix (takeExtension, takeBaseName)
import Control.Error.Util ((??))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Text.Pretty.Simple (pPrint)
import ProgressIndicator
  
initSubmit :: String -> Maybe String -> Maybe String -> IO ()
initSubmit file prob lang = do
  result <- runExceptT $ attemptSubmit file prob lang
  case result of
    Right v -> pPrint v
    Left e -> pPrint e

attemptSubmit :: String -> Maybe String -> Maybe String -> ExceptT AppError IO (String,String)
attemptSubmit file prob lang = do
  (p,l) <- (infer file prob lang) ?? AppError ("unable to infer information from " ++ file)
  return (p,l)

infer :: String -> Maybe String -> Maybe String -> Maybe (String, String)
infer file (Just p) (Just l) = return (p,l)
infer file Nothing Nothing = do
  p <- prob file
  l <- lang file
  return (p, l)
  where
    lang :: String -> Maybe String
    lang f = case takeExtension f of
      "" -> Nothing
      e -> Just (tail e) -- remove .
    prob :: String -> Maybe String
    prob f = case takeBaseName f of
      "" -> Nothing
      b -> return b

