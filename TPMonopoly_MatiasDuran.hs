module Library where
import Data.List
import Data.Char

type Propiedad = (String,Int)
type Accion = Participante -> Participante


data Participante = UnParticipante {
  nombre :: String,
  dinero :: Int,
  tactica :: String,
  propiedades :: [Propiedad],
  acciones :: [Accion]

}

carolina :: Participante
carolina = UnParticipante "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Participante
manuel = UnParticipante "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse]

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = unParticipante { dinero = dinero unParticipante + 40, tactica = "Comprador Compulsivo", acciones = acciones unParticipante ++ [pasarPorElBanco]}

agregarAccion :: Participante -> Accion -> Participante
agregarAccion unParticipante accion = unParticipante {acciones = acciones unParticipante ++ [accion] }

enojarse:: Accion
enojarse unParticipante = gritar (unParticipante {dinero = dinero unParticipante + 50, acciones = acciones unParticipante ++ [enojarse]})

gritar:: Accion
gritar unParticipante =  unParticipante {nombre = "AHHHH " ++ nombre unParticipante, acciones = acciones unParticipante ++ [gritar]}

cobrarAlquileres unParticipante = unParticipante {dinero = (dinero unParticipante + (10 * (propiedadesBaratas (propiedades unParticipante))) + (20 * (propiedadesCaras (propiedades unParticipante)))), acciones = acciones unParticipante ++ [cobrarAlquileres]}


propiedadesCaras:: [Propiedad] -> Int
propiedadesCaras props = length(filter (>=150) (map (obtenerValor) props))
propiedadesBaratas:: [Propiedad] -> Int
propiedadesBaratas props = length(filter (<150) (map (obtenerValor) props))

obtenerValor:: Propiedad -> Int
obtenerValor (_,valor) = valor


pagarAAccionistas unParticipante 
    | esAccionista unParticipante = unParticipante {dinero = dinero unParticipante + 200, acciones = acciones unParticipante ++ [pagarAAccionistas]}
    | otherwise                   = unParticipante {dinero = dinero unParticipante - 100, acciones = acciones unParticipante ++ [pagarAAccionistas]}

esAccionista unParticipante = tactica unParticipante == "Accionista"

tieneTactica :: String -> Participante -> Bool
tieneTactica unaTactica unParticipante = tactica unParticipante == unaTactica 

subastar:: Participante -> Propiedad -> Participante
subastar unParticipante unaPropiedad
      | tieneTactica "Oferente Singular" unParticipante = unParticipante {dinero = dinero unParticipante - (obtenerValor unaPropiedad), propiedades = propiedades unParticipante ++ [unaPropiedad]}
      | tieneTactica "Accionista" unParticipante = unParticipante {dinero = dinero unParticipante - (obtenerValor unaPropiedad), propiedades = propiedades unParticipante ++ [unaPropiedad]}
      | otherwise = unParticipante


