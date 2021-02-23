module IO_Helper(
  solicitarArchivo,
  solicitarNuevaPregunta,
  solicitarOpcion,
  solicitarPrediccion,
  solicitarRespuesta,
  solicitarRespuestaCorrecta,
  solicitarNuevaPrediccion,
  imprimirPrediccion,
  imprimirPregunta,
  imprimirPrediccionExitosa,
  imprimirArchivoErrado,
  imprimirOpcionErrada,
  imprimirOraculoErrado,
  prompt
) where
import Oraculo
import System.IO
import Data.Map as Map

solicitarOpcion :: IO String
solicitarOpcion = do
  putStrLn $ "\n" ++ "Introduzca una opción."
  putStrLn "1) Crear un Oráculo nuevo."
  putStrLn "2) Predecir."
  putStrLn "3) Persistir."
  putStrLn "4) Cargar."
  putStrLn "5) Consultar pregunta crucial."
  putStrLn "6) Salir."
  prompt

solicitarArchivo :: IO String
solicitarArchivo = do
  putStrLn $ "\n" ++ "Introduce el nombre del archivo."
  prompt

solicitarNuevaPrediccion :: IO String
solicitarNuevaPrediccion = do
  putStrLn $ "\n" ++ "Introduce una predicción."
  prompt

solicitarPrediccion :: IO String
solicitarPrediccion = do
  putStrLn $ "\n" ++ "He fallado! Cuál era la respuesta correcta?"
  prompt

solicitarRespuesta :: Oraculo -> IO String
solicitarRespuesta (Pregunta preg opcs) = do
  putStrLn $ "\n" ++ pregunta (Pregunta preg opcs)
  prompt

solicitarNuevaPregunta :: String -> IO String
solicitarNuevaPregunta prediccionCorrecta = do
  putStrLn $ "\n" ++ "Que pregunta distingue a " ++ prediccionCorrecta ++ " de las otras opciones?"
  prompt

solicitarRespuestaCorrecta :: String -> String -> IO String
solicitarRespuestaCorrecta nuevaPregunta prediccionCorrecta = do
  putStrLn $ "\n" ++ "Cuál es la respuesta a \"" ++ nuevaPregunta ++ "\" para " ++ prediccionCorrecta ++ "?"
  prompt

imprimirPrediccion :: Oraculo -> IO ()
imprimirPrediccion oraculo = do
  putStrLn $ "\n" ++ "Predicción: " ++ (prediccion oraculo) ++ " (Si/No)"

imprimirPregunta :: Oraculo -> IO ()
imprimirPregunta oraculo = do
  putStrLn $ "\n" ++ pregunta oraculo
  putStrLn $ Prelude.foldl concatenar "" (Prelude.map fst $ Map.toList $ opciones oraculo)
  where 
    concatenar ""        opcion = opcion
    concatenar acumulado opcion = acumulado ++ "/" ++ opcion

imprimirPrediccionExitosa :: IO ()
imprimirPrediccionExitosa = do
  imprimirExito "Asombrado? No lo dudo."

imprimirOraculoErrado :: IO ()
imprimirOraculoErrado = do
  imprimirError "Oráculo inválido."

imprimirOpcionErrada :: IO ()
imprimirOpcionErrada = do
  imprimirError "Opción inválida."

imprimirArchivoErrado :: IO ()
imprimirArchivoErrado = do
  imprimirError "Archivo inválido."

imprimirError :: String -> IO ()
imprimirError error = do putStrLn $ prefix ++ error ++ suffix
  where
    prefix = "\ESC[1;31m[ERROR] "
    suffix = "\ESC[0m"

imprimirExito :: String -> IO ()
imprimirExito mensaje = do putStrLn $ prefix ++ mensaje ++ suffix
  where
    prefix = "\ESC[1;32m[EXITO] "
    suffix = "\ESC[0m"

prompt :: IO String
prompt = do
  putStr "R: "
  hFlush stdout
  getLine