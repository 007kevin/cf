module AppError where

data AppError
  = AppError String
  | EParse String
  | EWriteFile String
  | EReadFile String  
  | FailedLogin
  deriving Show

