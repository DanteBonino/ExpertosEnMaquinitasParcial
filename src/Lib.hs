module Lib () where

--Datos que da el enunciado:

data Persona = Persona {
    nombre   :: String,
    dinero   :: Float,
    suerte   :: Int,
    factores :: [Factor]
}

--Tenemos un par de personas de ejemplo para poder probar nuestro programa:
nico = (Persona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)])
maiu = (Persona "Maiu" 100.0 42 [("inteligencia",55), ("paciencia",50)])

--Punto 1:
type Factor = (String, Int)
--Version 1 => Acá para no repetir la llamada a la suerte,  se agarra la suerte y se la multiplica x un valor según si tiene o no un Amuleto
suerteTotal :: Persona -> Int
suerteTotal unaPersona = (valorAmuleto unaPersona) * (suerte unaPersona) -- ((* (valorAmuleto unaPersona)) . suerte)

valorAmuleto :: Persona -> Int
valorAmuleto unaPersona
    | tieneAmuleto unaPersona =  (snd . amuleto) unaPersona
    | otherwise               = 1

tieneAmuleto :: Persona -> Bool --Inicialmente las siguientes funciones eran particulares, después en el punto 2 tuve que crear un tienePaciencia y, entonces, las generalicé para cualquier factor
tieneAmuleto = tieneFactor "amuleto"

amuleto :: Persona -> Factor
amuleto = factor "amuleto"

--Version 2 => Acá se calcula 2 veces la suerte, pero se necesita 1 función menos
suerteTotalV2 :: Persona -> Int
suerteTotalV2 unaPersona
    | tieneAmuleto unaPersona = (valorAmuletoV2 unaPersona) * (suerte unaPersona)
    | otherwise               = suerte unaPersona 
    
valorAmuletoV2 :: Persona -> Int
valorAmuletoV2 = (snd . amuleto) --No necesitas la guarda como en valorAmuleto xq ya sabes que tiene amuleto. Pero también es verdad que si alguien llama a la función sin que tenga amuleto y rompería

--Punto 2:
data Juego = Juego{
    nombreDelJuego            :: String,
    gananciaSegunMontoInicial :: Float -> Float,
    criteriosParaGanar        :: [Criterio]
}
type Criterio = Persona -> Bool

ruleta :: Juego
ruleta = Juego "Ruleta" (* 37) [suerteTotalMayorA 80]

suerteTotalMayorA :: Int -> Criterio
suerteTotalMayorA cotaInferior = ((> cotaInferior) . suerteTotal)

maquinita :: Float -> Juego
maquinita unJackPot = Juego "Maquinita" (+ unJackPot) [suerteTotalMayorA 95, tienePaciencia]

tienePaciencia :: Criterio
tienePaciencia = tieneFactor "paciencia"

tieneFactor :: String -> Persona -> Bool
tieneFactor unFactor = (any (esFactor unFactor) . factores)

esFactor :: String -> Factor -> Bool
esFactor unNombre (nombreDelFactor, valorDelFactor) = unNombre == nombreDelFactor && valorDelFactor > 0 

factor :: String -> Persona -> Factor
factor nombreDeUnFactor = (head . filter ((esFactor nombreDeUnFactor)) . factores)

--Punto 3:
ganaElJuego :: Persona -> Juego -> Bool
ganaElJuego unJugador = (all ($ unJugador) . criteriosParaGanar)

--Punto 4:
--a
dineroTotal :: Persona -> Float -> [Juego] -> Float
dineroTotal unaPersona unMontoInicial = (foldl (flip ($)) unMontoInicial . map (gananciaSegunMontoInicial) . filter (ganaElJuego unaPersona)
--a`
dineroTotal' unaPersona unMontoInicial = foldl jugarSiPuedeGanar unMontoInicial
  where jugarSiPuedeGanar unMonto unJuego
          | ganaElJuego unaPersona unJuego = gananciaSegunMontoInicial unJuego unMonto
          | otherwise                      = unMonto
{-
    La función de a` es lo mismo que hacer esto:
    dineroTotalV3 :: Foldable t => Persona -> Float -> t Juego -> Float
    dineroTotalV3 unaPersona unMontoInicial = foldl (jugarSiPuedeGanarV2 unaPersona) unMontoInicial

    jugarSiPuedeGanarV2 :: Persona -> Float -> Juego -> Float
    jugarSiPuedeGanarV2 unaPersona unMonto unJuego
        | ganaElJuego unaPersona unJuego = gananciaSegunMontoInicial unJuego unMonto
        | otherwise                      = unMonto
    O sea, aplicar parcialmente jugarSiPuedeGanar siempre con la misma Persona y modificar unMonto según si gana o no el juego.
-}
--b
dineroTotalConRecursividad :: Persona -> Float -> [Juego] -> Float
dineroTotalConRecursividad unaPersona unMontoInicial [] = unMontoInicial
dineroTotalConRecursividad unaPersona unMontoInicial (unJuego : restoDeJuegos)
    | ganaElJuego unaPersona unJuego = dineroTotalConRecursividad unaPersona (gananciaSegunMontoInicial unJuego unMontoInicial) restoDeJuegos
    | otherwise                      = dineroTotalConRecursividad unaPersona unMontoInicial restoDeJuegos

dineroTotalNico :: Float
dineroTotalNico = dineroTotal nico 400 [ruleta, maquinita 500]

dineroTotalMaiu :: Float
dineroTotalMaiu = dineroTotal maiu 400 [ruleta, maquinita 500]

--Punto 5:
nombresDePerdedores :: [Persona] -> [Juego] -> [String]
nombresDePerdedores unasPersonas  = (map nombre . perdedoresDeTodosLosJuegos unasPersonas)

perdedoresDeTodosLosJuegos :: [Persona] -> [Juego] -> [Persona]
perdedoresDeTodosLosJuegos unasPersonas unosJuegos = filter (noGananNinguno unosJuegos) unasPersonas

noGananNinguno :: [Juego] -> Persona -> Bool
noGananNinguno unosJuegos unaPersona = all (not . ganaElJuego unaPersona) unosJuegos

--Punto 6:
apostar :: Float -> Juego -> Persona -> Persona
apostar unMonto  unJuego = (aumentarDineroSiGana unJuego unMonto . reducirDinero unMonto)

reducirDinero :: Float -> Persona -> Persona
reducirDinero unMonto = modificarDinero (subtract unMonto)

modificarDinero :: (Float -> Float) -> Persona -> Persona
modificarDinero modificacion unJugador = unJugador {dinero = (modificacion . dinero) unJugador}

aumentarDineroSiGana :: Juego -> Float -> Persona -> Persona
aumentarDineroSiGana unJuego unMontoInicial unJugador
    | ganaElJuego unJugador unJuego = aumentarDinero (gananciaSegunMontoInicial unJuego unMontoInicial) unJugador
    | otherwise                     = unJugador

aumentarDinero :: Float -> Persona -> Persona
aumentarDinero unValor = modificarDinero (+ unValor)
