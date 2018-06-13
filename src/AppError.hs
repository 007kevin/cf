module AppError where

data AppError
  = AppError String
  | EParse String
  | EWriteFile FilePath String
  | FailedLogin
  deriving Show

