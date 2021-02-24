module Predicciones(
  realizarPrediccion, buscarLCA
) where
import Oraculo
import System.IO
import IO_Helper
import Data.Map as Map

-- Metodos de ayuda para predicciones
prediccionFallida :: Oraculo -> IO Oraculo
prediccionFallida oraculo = do
  prediccionCorrecta <- solicitarPrediccion
  nuevaPregunta <- solicitarNuevaPregunta prediccionCorrecta
  respuestaCorrecta <- solicitarRespuestaCorrecta nuevaPregunta prediccionCorrecta
  respuestaIncorrecta <- solicitarRespuestaCorrecta nuevaPregunta (prediccion oraculo)
  return $ ramificar [respuestaIncorrecta, respuestaCorrecta]
                    [oraculo, crearOraculo prediccionCorrecta]
                    nuevaPregunta

pedirRespuestaParaPrediccion :: Oraculo -> IO Oraculo
pedirRespuestaParaPrediccion oraculo = do
  opcion <- prompt
  case opcion of
    "Si" -> do
      imprimirPrediccionExitosa
      return $ oraculo
    "No" -> prediccionFallida oraculo
    _    -> do
          imprimirOpcionErrada
          manejarPrediccion oraculo 

manejarPrediccion :: Oraculo -> IO Oraculo
manejarPrediccion oraculo = do
  imprimirPrediccion oraculo
  pedirRespuestaParaPrediccion oraculo

-- Metodos de ayuda para preguntas
insertarPregunta :: Oraculo -> String -> Oraculo -> Oraculo
insertarPregunta (Pregunta preg opcs) opcion prediccion = Pregunta preg (insert opcion prediccion opcs)

validarOpcion :: Oraculo -> String -> Bool
validarOpcion (Pregunta preg opcs) opcion = Map.member opcion opcs

preguntaFallida :: Oraculo -> IO Oraculo
preguntaFallida oraculo = do
  prediccionCorrecta <- solicitarPrediccion
  opcion <- solicitarRespuesta oraculo
  return $ insertarPregunta oraculo opcion (crearOraculo prediccionCorrecta)

preguntaAcertada :: Oraculo -> String -> IO Oraculo
preguntaAcertada oraculoActual opcionEscogida = do
  oraculo <- realizarPrediccion $ respuesta oraculoActual opcionEscogida
  return $ insertarPregunta oraculoActual opcionEscogida oraculo 

pedirRespuestaParaPregunta :: Oraculo -> IO  Oraculo
pedirRespuestaParaPregunta oraculo = do
  opcion <- prompt
  case opcion of
    "ninguna" -> do
      preguntaFallida oraculo
    opcionEscogida -> do
      if (validarOpcion oraculo opcionEscogida)
        then preguntaAcertada oraculo opcionEscogida
        else do
          imprimirOpcionErrada
          manejarPregunta oraculo

manejarPregunta :: Oraculo -> IO Oraculo
manejarPregunta (Pregunta preg opcs) = do
  imprimirPregunta oraculo
  pedirRespuestaParaPregunta oraculo
  where 
    oraculo = Pregunta preg opcs

-- Metodo de prediccion:
realizarPrediccion :: Oraculo -> IO Oraculo
realizarPrediccion (Prediccion pred) = manejarPrediccion (Prediccion pred)
realizarPrediccion (Pregunta preg opcs) = manejarPregunta (Pregunta preg opcs)

-- LCA de dos nodos dados los caminos de cada uno desde la raiz
lca' :: [(Oraculo,a)] -> [(Oraculo,a)] -> Maybe (Oraculo,(a,a))
lca' [] _ = Nothing
lca' _ [] = Nothing
lca' ((ox,ix):xs) ((oy,iy):ys)
    | ox /= oy                            = Nothing
    | Prelude.null xs || Prelude.null ys  = Just (ox,(ix,iy))
    | (fst . head) xs == (fst . head) ys  = lca' xs ys
    | otherwise                           = Just (ox,(ix,iy))

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
            imprimirPreguntaCritica preg pred1 pred2 lca
          otherwise -> do
            imprimirPreguntaCritErr
    return oraculo