{-  
  Autores:
  - Denylson Romero 13-11270
  - Daniel Marin    10-10419
-}

module Haskinator(main) where
import Oraculo
import System.Exit
import System.IO
import Control.Exception
import qualified Data.Map as Map

menu :: Maybe Oraculo -> IO()
menu oraculo = do
  opcion <- solicitarOpcion
  oraculoNuevo <- case opcion of
    "1" -> crear
    "2" -> predecir $ oraculo
    "3" -> persistir $ oraculo
    "4" -> cargar
    "5" -> consultarPreguntaCritica $ oraculo
    "6" -> exitSuccess
    _   -> do
      imprimirOpcionErrada
      return oraculo 
  menu oraculoNuevo

{--------------------------------------------------
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
          Metodos principales del menu
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
----------------------------------------------------}

{- 
  crear/0
  solicita una nueva prediccion, genera un nuevo oraculo y lo retorna.
-}
crear :: IO (Maybe Oraculo)
crear = do
  prediccion <- solicitarNuevaPrediccion
  imprimirOraculoExitoso
  return $ Just $ crearOraculo prediccion

{- 
  persistir/1
  recibe el oraculo actual, solicita un nombre de archivo, persiste
  oraculo en disco y lo retorna.
-}
persistir :: Maybe Oraculo -> IO (Maybe Oraculo)
persistir (Just oraculo) = do
  nombreArchivo <- solicitarArchivo
  writeFile nombreArchivo (show oraculo)
  imprimirArchivoGuardado nombreArchivo
  return $ Just oraculo
persistir Nothing = do
  imprimirOraculoErrado
  return Nothing

{- 
  cargar/0
  Solicita un nombre de archivo y carga su contenido como el oraculo
  actual. En caso de que el archivo sea inválido, se solicita nuevamente
  el nombre del archivo.
-}
cargar :: IO (Maybe Oraculo)
cargar = catch exito fallo
  where
    fallo :: IOError -> IO (Maybe Oraculo)
    fallo _ = do
      imprimirArchivoErrado
      cargar
    exito :: IO (Maybe Oraculo)
    exito = do
      nombreArchivo <- solicitarArchivo
      archivo <- readFile nombreArchivo
      imprimirArchivoCorrecto
      return $ Just $ oraculo archivo
      where
        oraculo archivo = read archivo :: Oraculo

{- 
  predecir/1
  recibe el oraculo actual y realiza la prediccion sobre el.
-}
predecir :: Maybe Oraculo -> IO (Maybe Oraculo)
predecir (Just oraculo) = do
  resultado <- realizarPrediccion oraculo
  return $ Just $ resultado
predecir Nothing = do
  imprimirOraculoErrado
  return Nothing

{- 
  consultarPreguntaCritica/1
  recibe el oraculo actual e indica la pregunta que decide entre
  dos predicciones ingresadas por el usuario, junto a las respuestas 
  de esas dos predicciones
-}
consultarPreguntaCritica :: Maybe Oraculo -> IO (Maybe Oraculo)
consultarPreguntaCritica (Just o) = do
  p1 <- solicitarPrediccionCritica "primera"
  p2 <- solicitarPrediccionCritica "segunda"
  if p1==p2 
    then do
      imprimirPrediccionRepetida
      return $ Just o
  else do
    oraculo <- buscarLCA p1 p2 o
    return $ Just oraculo
consultarPreguntaCritica Nothing = do
  imprimirOraculoErrado
  return Nothing

{--------------------------------------------------
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
          Metodos de ayuda para predicciones
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
----------------------------------------------------}

{- 
  prediccionFallida/1
  Recibe un oraculo cuya prediccion ha fallado y consulta al usuario
  la prediccion correcta, la pregunta que la diferencia, la respuesta
  correcta e incorrecta. Con estos valores ramifica el oraculo.
-}
prediccionFallida :: Oraculo -> IO Oraculo
prediccionFallida oraculo = do
  prediccionCorrecta <- solicitarPrediccion
  nuevaPregunta <- solicitarNuevaPregunta prediccionCorrecta
  respuestaCorrecta <- solicitarRespuestaCorrecta nuevaPregunta prediccionCorrecta
  respuestaIncorrecta <- solicitarRespuestaCorrecta nuevaPregunta (prediccion oraculo)
  return $ ramificar [respuestaIncorrecta, respuestaCorrecta]
                    [oraculo, crearOraculo prediccionCorrecta]
                    nuevaPregunta

{- 
  pedirRespuestaParaPrediccion/1
  Recibe un oraculo y pide consulta al usuario si la prediccion
  fue correcta.
-}
pedirRespuestaParaPrediccion :: Oraculo -> IO Oraculo
pedirRespuestaParaPrediccion oraculo = do
  opcion <- promptUsuario
  case opcion of
    "Si" -> do
      imprimirPrediccionExitosa
      return $ oraculo
    "No" -> prediccionFallida oraculo
    _    -> do
          imprimirOpcionErrada
          manejarPrediccion oraculo 

{-  
  manejarPrediccion/1
  Recibe un oraculo y realiza la prediccion.
-}
manejarPrediccion :: Oraculo -> IO Oraculo
manejarPrediccion oraculo = do
  imprimirPrediccion oraculo
  pedirRespuestaParaPrediccion oraculo

{-  
  realizarPrediccion/1
  Recibe un oraculo y usa pattern matching para saber que funcion
  de manejo usar.
-}
realizarPrediccion :: Oraculo -> IO Oraculo
realizarPrediccion (Prediccion pred) = manejarPrediccion (Prediccion pred)
realizarPrediccion (Pregunta preg opcs) = manejarPregunta (Pregunta preg opcs)

{--------------------------------------------------
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
          Metodos de ayuda para preguntas
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
----------------------------------------------------}

{- 
  insertarPregunta/3
  Recibe un oraculo pregunta, una opcion y una prediccion, agrega
  la opcion y la prediccion como una nueva opcion del oraculo
  recibido.
-}
insertarPregunta :: Oraculo -> String -> Oraculo -> Oraculo
insertarPregunta (Pregunta preg opcs) opcion prediccion = Pregunta preg (Map.insert opcion prediccion opcs)

{- 
  validarOpcion/2
  Recibe un oraculo pregunta y una opcion, devuelve un booleano
  indicando si la opcion se encuentra entre las opciones del oraculo
  provisto.
-}
validarOpcion :: Oraculo -> String -> Bool
validarOpcion (Pregunta preg opcs) opcion = Map.member opcion opcs

{-
  preguntaFallida/1
  Recibe un oraculo de pregunta al que se respondio con "ninguna",
  se solicita la respuesta correcta y se agrega al oraculo.
-}
preguntaFallida :: Oraculo -> IO Oraculo
preguntaFallida oraculo = do
  prediccionCorrecta <- solicitarPrediccion
  opcion <- solicitarRespuesta oraculo
  return $ insertarPregunta oraculo opcion (crearOraculo prediccionCorrecta)

{- 
  preguntaAcertada/2
  Recibe un oraculo y una opcion, llama a realizarPrediccion para continuar con
  el siguiente nodo y agrega la opcion escogida al oraculo actual.
-}
preguntaAcertada :: Oraculo -> String -> IO Oraculo
preguntaAcertada oraculoActual opcionEscogida = do
  oraculo <- realizarPrediccion $ respuesta oraculoActual opcionEscogida
  return $ insertarPregunta oraculoActual opcionEscogida oraculo 

{- 
  pedirRespuestaParaPregunta/1
  Recibe un oraculo pregunta y se pide respuesta.
-}
pedirRespuestaParaPregunta :: Oraculo -> IO  Oraculo
pedirRespuestaParaPregunta oraculo = do
  opcion <- promptUsuario
  case opcion of
    "ninguna" -> do
      preguntaFallida oraculo
    opcionEscogida -> do
      if (validarOpcion oraculo opcionEscogida)
        then preguntaAcertada oraculo opcionEscogida
        else do
          imprimirOpcionErrada
          manejarPregunta oraculo

{-  
  manejarPregunta/1
  Recibe un oraculo y realiza la pregunta.
-}
manejarPregunta :: Oraculo -> IO Oraculo
manejarPregunta oraculo = do
  imprimirPregunta oraculo
  pedirRespuestaParaPregunta oraculo

--
{- 
  lca'/2
  LCA de dos nodos dados los caminos de cada uno desde la raiz
-}
lca' :: [(Oraculo,a)] -> [(Oraculo,a)] -> Maybe (Oraculo,(a,a))
lca' [] _ = Nothing
lca' _ [] = Nothing
lca' ((ox,ix):xs) ((oy,iy):ys)
    | ox /= oy                            = Nothing
    | null xs || null ys  = Just (ox,(ix,iy))
    | (fst . head) xs == (fst . head) ys  = lca' xs ys
    | otherwise                           = Just (ox,(ix,iy))

{- 
  buscarLCA/3
  Recibe dos strings y un oraculo y ejecuta la funcion lca'
-}
buscarLCA :: String -> String -> Oraculo -> IO Oraculo
buscarLCA pred1 pred2 oraculo = do
    let 
      navegarArbol opciones f                                = map (\(k,v) -> (f v, k)) (Map.toList opciones)
      fstNoEsNothing                                         = ((/=) Nothing) . fst  
      dirsHastaPredic pred oraculo@(Prediccion pred')        = if pred' == pred then Just [(oraculo,"")] else Nothing
      dirsHastaPredic pred oraculo@(Pregunta pred' opciones) = 
        case (filter fstNoEsNothing $ navegarArbol opciones $ dirsHastaPredic pred) of
          ((Just x,k):xs) -> Just ((oraculo,k):x)
          otherwise       -> Nothing 
    let camino1 = dirsHastaPredic pred1 oraculo
    let camino2 = dirsHastaPredic pred2 oraculo
    case (camino1, camino2) of
      (Nothing, Nothing) -> do
        prediccionCriticaInvalida2 pred1 pred2
      (Nothing, _) -> do 
        prediccionCriticaInvalida pred1
      (_, Nothing) -> do
        prediccionCriticaInvalida pred2
      (Just camino1, Just camino2) -> do
        let lca = maybe (Prediccion "",([],[])) id $ lca' camino1 camino2
        case fst lca of
          Pregunta preg _ -> do
            imprimirPreguntaCritica preg pred1 pred2 (fst $ snd lca) (snd $ snd lca)
          otherwise -> do
            imprimirPreguntaCritErr
    return oraculo

{--------------------------------------------------
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
        Metodos de ayuda para Input/Output
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
----------------------------------------------------}

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
  putStrLn $ foldl concatenar "" (map fst $ Map.toList $ opciones oraculo)
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
  imprimirOraculoExitoso/0
  Imprime un mensaje indicando que el oraclo fue creado exitosamente
-}
imprimirOraculoExitoso:: IO ()
imprimirOraculoExitoso = do
  imprimirExito "El oraculo ha sido creado."

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
  imprimirArchivoCorrecto/0
  Imprime un mensaje indicando que el archivo fue cargado correctamente
-}
imprimirArchivoCorrecto :: IO ()
imprimirArchivoCorrecto = do
  imprimirExito "Archivo cargado correctamente."

{- 
  imprimirArchivoGuardado/1
  Imprime un mensaje indicando que el archivo fue guardado correctamente con el
  nombre escrito por el usuario
-}
imprimirArchivoGuardado :: String -> IO ()
imprimirArchivoGuardado nombreArchivo = do
  imprimirExito $ "Archivo guardado correctamente con el nombre \"" ++ nombreArchivo ++ "\""

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

{--------------------------------------------------
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
                     Main
---------------------------------------------------
---------------------------------------------------
---------------------------------------------------
----------------------------------------------------}

main::IO()
main = menu Nothing
