module Predicciones(
  realizarPrediccion
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
  opcion <- getLine
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