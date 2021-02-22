module Oraculo(
  Oraculo (..), 
  Opciones,
  crearOraculo, 
  prediccion, 
  pregunta, 
  opciones, 
  respuesta,
  ramificar
) where
import Data.Map as Map

data Oraculo = Prediccion String | Pregunta String Opciones
                deriving(Show,Read)

type Opciones = Map String Oraculo

crearOraculo :: String -> Oraculo
crearOraculo s = Prediccion s

prediccion :: Oraculo -> String
prediccion (Prediccion s) = s
prediccion _ = error "Predicción inválida."

pregunta :: Oraculo -> String
pregunta (Pregunta s _) = s
pregunta _ = error "Pregunta inválida."

opciones :: Oraculo -> Opciones
opciones (Pregunta _ opciones) = opciones
opciones _ = error "Pregunta inválida."

respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta _ opciones) clave =
    case (Map.lookup clave opciones) of
        Just opcion -> opcion
        Nothing -> error "Respuesta inválida."
respuesta _ _ = error "Pregunta inválida."

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opciones oraculos pregunta =
  Pregunta pregunta $ fromList $ zip opciones oraculos