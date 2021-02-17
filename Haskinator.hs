module Haskinator(main) where
import Oraculo
import System.Exit
import System.IO
import Data.Map as Map

menu :: Maybe Oraculo -> IO()
menu oraculo = do
  putStrLn "Introduzca una opción."
  putStrLn "1) Crear un Oráculo nuevo."
  putStrLn "2) Predecir."
  putStrLn "3) Persistir."
  putStrLn "4) Cargar."
  putStrLn "5) Consultar pregunta crucial."
  putStrLn "6) Salir."
  opcion <- getLine
  oraculoNuevo <- case opcion of
    "1" -> crearNuevoOraculo
    -- "2" -> predecir
    "3" -> persistir oraculo
    "4" -> cargar
    -- "5" -> consultarPreguntaCritica
    "6" -> exitSuccess
    _ -> do
      putStrLn "Opción inválida."
      return oraculo 
  menu oraculoNuevo

crearNuevoOraculo :: IO (Maybe Oraculo)
crearNuevoOraculo = do
  putStrLn "Introduce una predicción:"
  prediccion <- getLine
  return $ Just $ crearOraculo prediccion

persistir :: Maybe Oraculo -> IO (Maybe Oraculo)
persistir (Just oraculo) = do
  putStrLn "Introduce el nombre del archivo."
  nombreArchivo <- getLine
  writeFile nombreArchivo (show oraculo)
  return $ Just oraculo
persistir Nothing = do
  putStrLn "Oráculo inválido."
  return Nothing

cargar :: IO (Maybe Oraculo)
cargar = do
    putStrLn "Introduce el archivo a leer."
    nombreArchivo <- getLine
    s <- readFile nombreArchivo
    return $ Just $ oraculo s
  where
    oraculo s = read s :: Oraculo

{- 
predecir :: IO()
predecir = do
  putStrLn "Predecir"

consultarPreguntaCritica :: IO()
consultarPreguntaCritica = do
  putStrLn "Consultar pregunta crítica." 
-}

main::IO()
main = menu Nothing