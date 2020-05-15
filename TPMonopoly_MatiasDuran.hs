import Text.Show.Functions()

type Propiedad = (String,Int)
type Accion = Participante -> Participante


data Participante = UnParticipante {
  nombre :: String,
  dinero :: Int,
  tactica :: String,
  propiedades :: [Propiedad],
  acciones :: [Accion]

} deriving (Show)

carolina :: Participante
carolina = UnParticipante "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Participante
manuel = UnParticipante "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse]

puedeComprarPropiedad:: Propiedad -> Participante -> Bool
puedeComprarPropiedad unaPropiedad unParticipante = dinero unParticipante >= (obtenerPrecio unaPropiedad)

cambiarTactica :: String -> Participante -> Participante
cambiarTactica unaTactica unParticipante = unParticipante {tactica = unaTactica}

obtenerPrecio:: Propiedad -> Int
obtenerPrecio (_,precio) = precio

agregarDinero :: Int -> Accion
agregarDinero monto unParticipante = unParticipante {dinero = dinero unParticipante + monto}

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = (agregarDinero 40 . cambiarTactica "Comprador Compulsivo") unParticipante

enojarse:: Accion
enojarse unParticipante = gritar (agregarDinero 50 unParticipante)

gritar:: Accion
gritar unParticipante =  unParticipante {nombre = "AHHHH " ++ nombre unParticipante}

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = agregarDinero ((10 * (propiedadesBaratas (propiedades unParticipante))) + (20 * (propiedadesCaras (propiedades unParticipante)))) unParticipante 


propiedadesCaras:: [Propiedad] -> Int
propiedadesCaras props = (length.(filter (>=150)).(map (obtenerPrecio))) props
propiedadesBaratas:: [Propiedad] -> Int
propiedadesBaratas props = length(filter (<150) (map (obtenerPrecio) props))

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante 
    | tieneTactica "Accionista" unParticipante = agregarDinero 200 unParticipante
    | otherwise                                = agregarDinero (-100) unParticipante

tieneTactica :: String -> Participante -> Bool
tieneTactica unaTactica unParticipante = tactica unParticipante == unaTactica 

subastar:: Participante -> Propiedad -> Participante
subastar unParticipante unaPropiedad
      | tieneTactica "Oferente Singular" unParticipante || tieneTactica "Accionista" unParticipante = comprarPropiedad unaPropiedad unParticipante
      | otherwise = unParticipante

comprarPropiedad:: Propiedad -> Accion
comprarPropiedad unaPropiedad unParticipante = unParticipante{dinero = dinero unParticipante - (obtenerPrecio unaPropiedad), propiedades = propiedades unParticipante ++ [unaPropiedad]}

hacerBerrinchePor:: Propiedad -> Participante -> Participante
hacerBerrinchePor unaPropiedad unParticipante
      | puedeComprarPropiedad unaPropiedad unParticipante = (agregarDinero (obtenerPrecio unaPropiedad) . comprarPropiedad unaPropiedad) unParticipante
      | otherwise                                          = (hacerBerrinchePor unaPropiedad . agregarDinero 10) unParticipante

ultimaRonda :: Participante -> Participante -> Participante
ultimaRonda unParticipante = foldl1 (.) (acciones unParticipante)

juegoFinal :: Participante -> Participante -> Participante
juegoFinal primerParticipante segundoParticipante
      |  dineroEnLaUltimaRonda primerParticipante >= dineroEnLaUltimaRonda segundoParticipante = primerParticipante
      |  otherwise                                                                             = segundoParticipante

dineroEnLaUltimaRonda :: Participante -> Int
dineroEnLaUltimaRonda unParticipante = dinero ((ultimaRonda unParticipante) unParticipante)
