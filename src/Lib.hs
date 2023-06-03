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

tieneAmuleto :: Persona -> Bool
tieneAmuleto = ((not . null). amuleto)

amuleto :: Persona -> Factor
amuleto = (head . filter ((esAmuleto)) . factores)

esAmuleto :: Factor -> Bool
esAmuleto (nombre, valor) = nombre == "amuleto" && valor > 0 -- Por enunciado se considera que si el valor es 0, entonces no tiene amuleto

--Version 2 => Acá se calcula 2 veces la suerte, pero se necesita 1 función menos
suerteTotalV2 :: Persona -> Int
suerteTotalV2 unaPersona
    | tieneAmuleto unaPersona = (valorAmuletoV2 unaPersona) * (suerte unaPersona)
    | otherwise               = suerte unaPersona 
    
valorAmuletoV2 :: Persona -> Int
valorAmuletoV2 = (snd . amuleto) --No necesitas la guarda como en valorAmuleto xq ya sabes que tiene amuleto. Pero también es verdad que si alguien llama a la función sin que tenga amuleto y rompería


