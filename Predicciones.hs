module Predicciones(
  realizarPrediccion
) where
import Oraculo
import System.IO
import IO_Helper
import Data.Map as Map

-- Metodos de ayuda para predicciones
prediccionFallida :: Oraculo -> IO (Maybe Oraculo)
prediccionFallida oraculo = do
  prediccionCorrecta <- solicitarPrediccion
  nuevaPregunta <- solicitarNuevaPregunta prediccionCorrecta
  respuestaCorrecta <- solicitarRespuestaCorrecta nuevaPregunta prediccionCorrecta
  respuestaIncorrecta <- solicitarRespuestaCorrecta nuevaPregunta (prediccion oraculo)
  return $ Just $ ramificar  [respuestaIncorrecta, respuestaCorrecta]
                    [oraculo, crearOraculo prediccionCorrecta]
                    nuevaPregunta

pedirRespuestaParaPrediccion :: Oraculo -> IO (Maybe Oraculo)
pedirRespuestaParaPrediccion oraculo = do
  opcion <- getLine
  case opcion of
    "Si" -> do
      imprimirPrediccionExitosa
      return $ Just oraculo
    "No" -> prediccionFallida oraculo
    _    -> do
          imprimirOpcionErrada
          manejarPrediccion oraculo 

manejarPrediccion :: Oraculo -> IO (Maybe Oraculo)
manejarPrediccion oraculo = do
  imprimirPrediccion oraculo
  pedirRespuestaParaPrediccion oraculo

-- Metodos de ayuda para preguntas
insertarPregunta :: Oraculo -> String -> Maybe Oraculo -> Oraculo
insertarPregunta (Pregunta preg opcs) opcion (Just prediccion) = Pregunta preg (insert opcion prediccion opcs)

validarOpcion :: Oraculo -> String -> Bool
validarOpcion (Pregunta preg opcs) opcion = Map.member opcion opcs

preguntaFallida :: Oraculo -> IO (Maybe Oraculo)
preguntaFallida oraculo = do
  prediccionCorrecta <- solicitarPrediccion
  opcion <- solicitarRespuesta oraculo
  return $ Just $ insertarPregunta oraculo opcion (Just $ crearOraculo prediccionCorrecta)

preguntaAcertada :: Oraculo -> String -> IO (Maybe Oraculo)
preguntaAcertada oraculoActual opcionEscogida = do
  oraculo <- realizarPrediccion $ Just $ respuesta oraculoActual opcionEscogida
  return $ Just $ insertarPregunta oraculoActual opcionEscogida oraculo 

pedirRespuestaParaPregunta :: Oraculo -> IO (Maybe Oraculo)
pedirRespuestaParaPregunta oraculo = do
  opcion <- getLine
  case opcion of
    "ninguna" -> do
      preguntaFallida oraculo
    opcionEscogida -> do
      case (validarOpcion oraculo opcionEscogida) of
        True -> preguntaAcertada oraculo opcionEscogida
        False -> do
          imprimirOpcionErrada
          manejarPregunta oraculo

manejarPregunta :: Oraculo -> IO (Maybe Oraculo)
manejarPregunta (Pregunta preg opcs) = do
  imprimirPregunta oraculo
  pedirRespuestaParaPregunta oraculo
  where 
    oraculo = Pregunta preg opcs

-- Metodo de prediccion:
realizarPrediccion :: Maybe Oraculo -> IO (Maybe Oraculo)
realizarPrediccion (Just (Prediccion pred)) = manejarPrediccion (Prediccion pred)
realizarPrediccion (Just (Pregunta preg opcs)) = manejarPregunta (Pregunta preg opcs)
realizarPrediccion Nothing = do
  imprimirOraculoErrado
  return Nothing