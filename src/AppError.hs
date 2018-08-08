module AppError where

import Control.Exception
  
data AppError
  = AppError String
  | EParse String
  | EWriteFile String
  | EReadFile String
  | EConfig String
  | FailedLogin
  | FailedSubmission
  deriving Show

instance Exception AppError



