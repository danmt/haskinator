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
  solicitarPrediccionCri1,
  solicitarPrediccionCri2,
  imprimirPrediccionRepetida,
  prediccionCriticaInvalida,
  prediccionCriticaInvalida2,
  imprimirPreguntaCritErr,
  imprimirPreguntaCritica,
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

solicitarPrediccionCri1 :: IO String
solicitarPrediccionCri1 = do
  putStrLn $ "\n" ++ "Ingrese la primera predicción a consultar:"
  prompt

solicitarPrediccionCri2 :: IO String
solicitarPrediccionCri2 = do
  putStrLn $ "\n" ++ "Ingrese la segunda predicción a consultar:"
  prompt

prediccionCriticaInvalida :: String -> IO ()
prediccionCriticaInvalida prediccion = do
  putStrLn $ "Lo siento, pero \"" ++ prediccion ++"\" no es una prediccion correcta."

prediccionCriticaInvalida2 :: String -> String -> IO ()
prediccionCriticaInvalida2 pred1 pred2 = do
  putStrLn $ "Lo siento, pero las predicciones \"" ++ pred1 ++"\" y \"" ++ pred2 ++ "\" no son predicciones correctas"

imprimirPrediccionRepetida :: IO ()
imprimirPrediccionRepetida = do
  putStrLn $ "Las predicciones son iguales, dime dos predicciones diferentes."

imprimirPreguntaCritErr :: IO ()
imprimirPreguntaCritErr = do
  putStrLn "No hay pregunta que diferencie ambas predicciones."

imprimirPreguntaCritica :: String -> String -> String -> (Oraculo, ([Char], [Char])) -> IO ()
imprimirPreguntaCritica preg pred1 pred2 lca = do
  putStrLn $ "\nLa pregunta crucial para ambas predicciones es:\n" ++ preg
  putStrLn $ "La respuesta para \"" ++ pred1 ++ "\" es: " ++ (fst $ snd lca)
  putStrLn $ "La respuesta para \"" ++ pred2 ++ "\" es: " ++ (snd $ snd lca)
  

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