module Portuga where

import Parser
import Viz
import System.Environment
import Text.Parsec.String

parseFirst (x:_) = (parseFromFile portuga x) >>= (\(Right x)->return $ viz x) >>= writeFile (x ++ ".dot")

main = getArgs >>= parseFirst

