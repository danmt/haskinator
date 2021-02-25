{-  
  Autores:
  - Denylson Romero 13-11270
  - Daniel Marin    10-10419
-}

module Predicciones(
  realizarPrediccion, buscarLCA
) where
import Oraculo
import System.IO
import IO_Helper
import Data.Map as Map

-- Metodos de ayuda para predicciones

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

-- Metodos de ayuda para preguntas

{- 
  insertarPregunta/3
  Recibe un oraculo pregunta, una opcion y una prediccion, agrega
  la opcion y la prediccion como una nueva opcion del oraculo
  recibido.
-}
insertarPregunta :: Oraculo -> String -> Oraculo -> Oraculo
insertarPregunta (Pregunta preg opcs) opcion prediccion = Pregunta preg (insert opcion prediccion opcs)

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
  manejarPrediccion/1
  Recibe un oraculo y realiza la pregunta.
-}
manejarPregunta :: Oraculo -> IO Oraculo
manejarPregunta oraculo = do
  imprimirPregunta oraculo
  pedirRespuestaParaPregunta oraculo

-- Metodo de prediccion:

{-  
  realizarPrediccion/1
  Recibe un oraculo y usa pattern matching para saber que funcion
  de manejo usar.
-}
realizarPrediccion :: Oraculo -> IO Oraculo
realizarPrediccion (Prediccion pred) = manejarPrediccion (Prediccion pred)
realizarPrediccion (Pregunta preg opcs) = manejarPregunta (Pregunta preg opcs)

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
    | Prelude.null xs || Prelude.null ys  = Just (ox,(ix,iy))
    | (fst . head) xs == (fst . head) ys  = lca' xs ys
    | otherwise                           = Just (ox,(ix,iy))

{- 
  buscarLCA/3
  Recibe dos strings y un oraculo y ejecuta la funcion lca'
-}
buscarLCA :: String -> String -> Oraculo -> IO Oraculo
buscarLCA pred1 pred2 oraculo = do
    let 
      navegarArbol opciones f                                = Prelude.map (\(k,v) -> (f v, k)) (Map.toList opciones)
      fstNoEsNothing                                         = ((/=) Nothing) . fst  
      dirsHastaPredic pred oraculo@(Prediccion pred')        = if pred' == pred then Just [(oraculo,"")] else Nothing
      dirsHastaPredic pred oraculo@(Pregunta pred' opciones) = 
        case (Prelude.filter fstNoEsNothing $ navegarArbol opciones $ dirsHastaPredic pred) of
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