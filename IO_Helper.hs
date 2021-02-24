{-  
  Autores:
  - Denylson Romero 13-11270
  - Daniel Marin    10-10419
-}

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
  solicitarPrediccionCritica1,
  solicitarPrediccionCritica2,
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

{- 
  solicitarOpcion/0
  Imprime en pantalla las opciones disponibles y
  pide al usuario una opcion valida.
-}
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

{- 
  solicitarArchivo/0
  Solicita al usuario un nombre de archivo.
-}
solicitarArchivo :: IO String
solicitarArchivo = do
  putStrLn $ "\n" ++ "Introduce el nombre del archivo."
  prompt

{- 
  solicitarNuevaPrediccion/0
  Solicita al usuario una nueva predicción.
-}
solicitarNuevaPrediccion :: IO String
solicitarNuevaPrediccion = do
  putStrLn $ "\n" ++ "Introduce una predicción."
  prompt

{- 
  solicitarPrediccion/0
  Solicita al usuario la predicción.
-}
solicitarPrediccion :: IO String
solicitarPrediccion = do
  putStrLn $ "\n" ++ "He fallado! Cuál era la respuesta correcta?"
  prompt

{- 
  solicitarRespuesta/1
  Recibe un oraculo, imprime la pregunta y solicita al usuario la respuesta.
-}
solicitarRespuesta :: Oraculo -> IO String
solicitarRespuesta (Pregunta preg opcs) = do
  putStrLn $ "\n" ++ pregunta (Pregunta preg opcs)
  prompt

{- 
  solicitarNuevaPregunta/1
  Recibe una prediccion y solicita al usuario una pregunta que distinga
  la prediccion correcta.
-}
solicitarNuevaPregunta :: String -> IO String
solicitarNuevaPregunta prediccionCorrecta = do
  putStrLn $ "\n" ++ "Que pregunta distingue a " ++ prediccionCorrecta ++ " de las otras opciones?"
  prompt

{- 
  solicitarRespuestaCorrecta/2
  Recibe una pregunta y una prediccion, solicita al usuario
  la respuesta a la pregunta para la prediccion provista.
-}
solicitarRespuestaCorrecta :: String -> String -> IO String
solicitarRespuestaCorrecta preg pred = do
  putStrLn $ "\n" ++ "Cuál es la respuesta a \"" ++ preg ++ "\" para " ++ pred ++ "?"
  prompt

{- 
  imprimirPrediccion/1
  Recibe un oraculo prediccion y lo imprime por pantalla.
-}
imprimirPrediccion :: Oraculo -> IO ()
imprimirPrediccion oraculo = do
  putStrLn $ "\n" ++ "Predicción: " ++ (prediccion oraculo) ++ " (Si/No)"

{- 
  imprimirPrediccion/1
  Recibe un oraculo pregunta y la imprime por pantalla.
-}
imprimirPregunta :: Oraculo -> IO ()
imprimirPregunta oraculo = do
  putStrLn $ "\n" ++ pregunta oraculo
  putStrLn $ Prelude.foldl concatenar "" (Prelude.map fst $ Map.toList $ opciones oraculo)
  where 
    concatenar ""        opcion = opcion
    concatenar acumulado opcion = acumulado ++ "/" ++ opcion

{- 
  solicitarPrediccionCritica1/0
  Solicita al usuario la primera prediccion para consulta de pregunta critica.
-}
solicitarPrediccionCritica1 :: IO String
solicitarPrediccionCritica1 = do
  putStrLn $ "\n" ++ "Ingrese la primera predicción a consultar:"
  prompt

{- 
  solicitarPrediccionCritica2/0
  Solicita al usuario la segunda prediccion para consulta de pregunta critica.
-}
solicitarPrediccionCritica2 :: IO String
solicitarPrediccionCritica2 = do
  putStrLn $ "\n" ++ "Ingrese la segunda predicción a consultar:"
  prompt

{- 
  prediccionCriticaInvalida/1
  Recibe una prediccion e imprime un mensaje indicando que es incorrecta.
-}
prediccionCriticaInvalida :: String -> IO ()
prediccionCriticaInvalida prediccion = do
  putStrLn $ "Lo siento, pero \"" ++ prediccion ++"\" no es una prediccion correcta."

{- 
  prediccionCriticaInvalida2/1
  Recibe dos prediccion e imprime un mensaje indicando que ambas
  predicciones son incorrectas.
-}
prediccionCriticaInvalida2 :: String -> String -> IO ()
prediccionCriticaInvalida2 pred1 pred2 = do
  putStrLn $ "Lo siento, pero las predicciones \"" ++ pred1 ++"\" y \"" ++ pred2 ++ "\" no son predicciones correctas"

{- 
  imprimirPrediccionRepetida/0
  Imprime mensaje indicando que las predicciones son iguales.
-}
imprimirPrediccionRepetida :: IO ()
imprimirPrediccionRepetida = do
  putStrLn $ "Las predicciones son iguales, dime dos predicciones diferentes."

{- 
  imprimirPreguntaCritErr/0
  Imprime mensaje indicando que no hay pregunta que diferencie las predicciones.
-}
imprimirPreguntaCritErr :: IO ()
imprimirPreguntaCritErr = do
  putStrLn "No hay pregunta que diferencie ambas predicciones."

{- 
  imprimirPreguntaCritica/4
  Recibe una pregunta, dos predicciones y un lca, imprime la pregunta
  crucial para ambas predicciones.
-}
imprimirPreguntaCritica :: String -> String -> String -> (Oraculo, ([Char], [Char])) -> IO ()
imprimirPreguntaCritica preg pred1 pred2 lca = do
  putStrLn $ "\nLa pregunta crucial para ambas predicciones es:\n" ++ preg
  putStrLn $ "La respuesta para \"" ++ pred1 ++ "\" es: " ++ (fst $ snd lca)
  putStrLn $ "La respuesta para \"" ++ pred2 ++ "\" es: " ++ (snd $ snd lca)
  
{- 
  imprimirPrediccionExitosa/0
  Imprime un mensaje indicando la prediccion exitosa.
-}
imprimirPrediccionExitosa :: IO ()
imprimirPrediccionExitosa = do
  imprimirExito "Asombrado? No lo dudo."

{- 
  imprimirOraculoErrado/0
  Imprime un mensaje indicando que el oraculo es invalido.
-}
imprimirOraculoErrado :: IO ()
imprimirOraculoErrado = do
  imprimirError "Oráculo inválido."

{- 
  imprimirOpcionErrada/0
  Imprime un mensaje indicando que la opcion es invalida.
-}
imprimirOpcionErrada :: IO ()
imprimirOpcionErrada = do
  imprimirError "Opción inválida."

{- 
  imprimirArchivoErrado/0
  Imprime un mensaje indicando que el nombre del archivo es invalido.
-}
imprimirArchivoErrado :: IO ()
imprimirArchivoErrado = do
  imprimirError "Archivo inválido."

{- 
  imprimirError/1
  Recibe un string y lo imprime en la consola en color rojo.
-}
imprimirError :: String -> IO ()
imprimirError error = do putStrLn $ prefix ++ error ++ suffix
  where
    prefix = "\ESC[1;31m[ERROR] "
    suffix = "\ESC[0m"

{- 
  imprimirExito/1
  Recibe un string y lo imprime en la consola en color verde.
-}
imprimirExito :: String -> IO ()
imprimirExito mensaje = do putStrLn $ prefix ++ mensaje ++ suffix
  where
    prefix = "\ESC[1;32m[EXITO] "
    suffix = "\ESC[0m"

{- 
  prediccionCriticaInvalida/1
  Recibe una prediccion e imprime un mensaje indicando que es incorrecta.
-}
prompt :: IO String
prompt = do
  putStr "R: "
  hFlush stdout
  getLine