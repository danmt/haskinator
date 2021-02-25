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
  solicitarPrediccionCritica,
  imprimirPrediccionRepetida,
  prediccionCriticaInvalida,
  prediccionCriticaInvalida2,
  imprimirPreguntaCritErr,
  imprimirPreguntaCritica,
  promptUsuario,
  haskinatorDice
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
  promptUsuario

{- 
  solicitarArchivo/0
  Solicita al usuario un nombre de archivo.
-}
solicitarArchivo :: IO String
solicitarArchivo = do
  haskinatorDice $ "Introduce el nombre del archivo."
  promptUsuario

{- 
  solicitarNuevaPrediccion/0
  Solicita al usuario una nueva predicción.
-}
solicitarNuevaPrediccion :: IO String
solicitarNuevaPrediccion = do
  haskinatorDice $ "Introduce una predicción."
  promptUsuario

{- 
  solicitarPrediccion/0
  Solicita al usuario la predicción.
-}
solicitarPrediccion :: IO String
solicitarPrediccion = do
  haskinatorDice $ "He fallado! Cuál era la respuesta correcta?"
  promptUsuario

{- 
  solicitarRespuesta/1
  Recibe un oraculo, imprime la pregunta y solicita al usuario la respuesta.
-}
solicitarRespuesta :: Oraculo -> IO String
solicitarRespuesta (Pregunta preg opcs) = do
  haskinatorDice $ pregunta (Pregunta preg opcs)
  promptUsuario

{- 
  solicitarNuevaPregunta/1
  Recibe una prediccion y solicita al usuario una pregunta que distinga
  la prediccion correcta.
-}
solicitarNuevaPregunta :: String -> IO String
solicitarNuevaPregunta prediccionCorrecta = do
  haskinatorDice $ "Que pregunta distingue a " ++ prediccionCorrecta ++ " de las otras opciones?"
  promptUsuario

{- 
  solicitarRespuestaCorrecta/2
  Recibe una pregunta y una prediccion, solicita al usuario
  la respuesta a la pregunta para la prediccion provista.
-}
solicitarRespuestaCorrecta :: String -> String -> IO String
solicitarRespuestaCorrecta preg pred = do
  haskinatorDice $ "Cuál es la respuesta a \"" ++ preg ++ "\" para " ++ pred ++ "?"
  promptUsuario

{- 
  imprimirPrediccion/1
  Recibe un oraculo prediccion y lo imprime por pantalla.
-}
imprimirPrediccion :: Oraculo -> IO ()
imprimirPrediccion oraculo = do
  haskinatorDice $ "Predicción: " ++ (prediccion oraculo) ++ "\n(Si/No)"

{- 
  imprimirPrediccion/1
  Recibe un oraculo pregunta y la imprime por pantalla.
-}
imprimirPregunta :: Oraculo -> IO ()
imprimirPregunta oraculo = do
  haskinatorDice $ pregunta oraculo
  putStrLn $ Prelude.foldl concatenar "" (Prelude.map fst $ Map.toList $ opciones oraculo)
  where 
    concatenar ""        opcion = opcion
    concatenar acumulado opcion = acumulado ++ "/" ++ opcion

{- 
  solicitarPrediccionCritica/1
  Recibe como argumento el numero de prediccion (primera o segunda) que se le 
  solicitara al usuario para la consulta de pregunta critica.
-}
solicitarPrediccionCritica :: String -> IO String
solicitarPrediccionCritica nroPrediccion = do
  haskinatorDice $ "Ingrese la " ++ nroPrediccion ++ " predicción a consultar:"
  promptUsuario

{- 
  prediccionCriticaInvalida/1
  Recibe una prediccion e imprime un mensaje indicando que es incorrecta.
-}
prediccionCriticaInvalida :: String -> IO ()
prediccionCriticaInvalida prediccion = do
  imprimirError $ "La prediccion \"" ++ prediccion ++"\" no es una prediccion correcta."

{- 
  prediccionCriticaInvalida2/2
  Recibe dos predicciones e imprime un mensaje indicando que ambas
  predicciones son incorrectas.
-}
prediccionCriticaInvalida2 :: String -> String -> IO ()
prediccionCriticaInvalida2 pred1 pred2 = do
  imprimirError $ "Las predicciones \"" ++ pred1 ++"\" y \"" ++ pred2 ++ "\" no son predicciones correctas"

{- 
  imprimirPrediccionRepetida/0
  Imprime mensaje indicando que las predicciones son iguales.
-}
imprimirPrediccionRepetida :: IO ()
imprimirPrediccionRepetida = do
  imprimirError "Las predicciones son iguales, dime dos predicciones diferentes."

{- 
  imprimirPreguntaCritErr/0
  Imprime mensaje indicando que no hay pregunta que diferencie las predicciones.
-}
imprimirPreguntaCritErr :: IO ()
imprimirPreguntaCritErr = do
  haskinatorDice "No hay pregunta que diferencie ambas predicciones."

{- 
  imprimirPreguntaCritica/5
  Recibe una pregunta, dos predicciones y dos respuestas a la pregunta crucial, 
  imprime la pregunta crucial para ambas predicciones con sus respectivas respuestas.
-}
imprimirPreguntaCritica :: String -> String -> String -> String -> String -> IO ()
imprimirPreguntaCritica preg pred1 pred2 resp1 resp2 = do
  imprimirExito $ "\nLa pregunta crucial para ambas predicciones es:\n" ++ preg ++
    "\n" ++ "La respuesta para \"" ++ pred1 ++ "\" es: " ++ resp1 ++
    "\n" ++ "La respuesta para \"" ++ pred2 ++ "\" es: " ++ resp2
  
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
  promptUsuario/0
  Imprime un mensaje de espera de input por el usuario
-}
promptUsuario :: IO String
promptUsuario = do
  putStr "Usuario: "
  hFlush stdout
  getLine

{- 
  haskinatorDice/1
  Recibe un string y lo imprime en pantalla como Haskinator
-}
haskinatorDice :: String -> IO ()
haskinatorDice mensaje = do 
  putStrLn $ "\n" ++ "Haskinator: " ++ mensaje