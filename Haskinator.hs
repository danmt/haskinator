module Haskinator(main) where
import System.Exit
import System.IO

menu :: Maybe a -> IO()
menu _ = do
        putStrLn "Introduzca una opción."

main::IO()
main = menu Nothing