module AppError where

import Control.Exception
  
data AppError
  = AppError String
  | EParse String
  | EWriteFile String
  | EReadFile String
  | EConfig String
  | FailedLogin
  | FailedSubmission String
  deriving Show

instance Exception AppError



