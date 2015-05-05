module Portuga where

import Parser
import Viz
import System.Environment
import Text.Parsec.String

parseFiles [] = return ()
parseFiles (x:xs) = do parse <- parseFromFile portuga x
                       case parse of
                           Left err  -> print err
                           Right ast -> do writeFile (x ++ ".dot") (viz ast)
                                           parseFiles xs

main = getArgs >>= parseFiles

