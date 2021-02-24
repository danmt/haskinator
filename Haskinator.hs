{-  
  Autores:
  - Denylson Romero 13-11270
  - Daniel Marin    10-10419
-}

module Haskinator(main) where
import Oraculo
import IO_Helper
import Predicciones
import System.Exit
import System.IO
import Data.Map as Map
import Control.Exception
import Prelude

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

{- 
  crear/0
  solicita una nueva prediccion, genera un nuevo oraculo y lo retorna.
-}
crear :: IO (Maybe Oraculo)
crear = do
  prediccion <- solicitarNuevaPrediccion
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
  p1 <- solicitarPrediccionCritica1
  p2 <- solicitarPrediccionCritica2
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

main::IO()
main = menu Nothing
