module ProgressIndicator where

import Control.Concurrent
import Data.List
import System.IO

output message = sequence_ . intersperse (threadDelay 150000) . map (hPutStr stderr . ("\r\ESC[K"++) . (:' ':message)) $ cycle "-\\|/"
main = do
  output "Loading..."
