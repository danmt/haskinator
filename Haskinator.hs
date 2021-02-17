module Haskinator(main) where
import System.Exit
import System.IO

menu :: IO()
menu = do
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
    "2" -> predecir
    "3" -> persistir
    "4" -> cargar
    "5" -> consultarPreguntaCritica
    "6" -> exitSuccess
    _ -> do
      putStrLn "Opción incorrecta."
  menu

crearNuevoOraculo :: IO()
crearNuevoOraculo = do
  putStrLn "Introduzca una predicción."

predecir :: IO()
predecir = do
  putStrLn "Predecir"

persistir :: IO()
persistir = do
  putStrLn "Introduzca el archivo para guardar el oráculo."

cargar :: IO()
cargar = do
  putStrLn "Introduzca el archivo a leer."

consultarPreguntaCritica :: IO()
consultarPreguntaCritica = do
  putStrLn "Consultar pregunta crítica."

main::IO()
main = menu