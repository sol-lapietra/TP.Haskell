
import Test.HUnit
import Data.List
import Solucion

runCatedraTests = runTestTT allTests

allTests = test [
    "vuelosValidos" ~: testsEjvuelosValidos,
    "ciudadesConectadas" ~: testsEjciudadesConectadas,
    "modernizarFlota" ~: testsEjmodernizarFlota,
    "ciudadMasConectada" ~: testsEjciudadMasConectada,
    "sePuedeLlegar" ~: testsEjsePuedeLlegar,
    "duracionDelCaminoMasRapido" ~: testsEjduracionDelCaminoMasRapido,
    "puedoVolverAOrigen" ~: testsEjpuedoVolverAOrigen
    ]

testsEjvuelosValidos = test [         --YA LO REVISE
    "vuelo válido con un solo elemento" ~: vuelosValidos [("BsAs", "Rosario", 5.0)] ~?= True, 
    "no hay vuelos disponibles"~: vuelosValidos [] ~?= True,
    "vuelos de ida y vuelta" ~: vuelosValidos [("BsAs", "Rosario", 5.0), ("Rosario", "BsAs", 5.0)] ~?= True,
    "vuelos válidos con múltiples rutas" ~:vuelosValidos [("BsAs","Rosario",5.3), ("Bariloche","BsAs",2.0), ("Salta","Jujuy",5.0)] ~?= True, 
    "vuelo con origen y destino iguales " ~: vuelosValidos [("BsAs", "BsAs", 1.0)] ~?= False, 
    "vuelo con duración cero" ~: vuelosValidos [("BsAs", "Rosario", 0.0)] ~?= False, 
    "vuelos repetidos" ~: vuelosValidos [("BsAs","Rosario",5.0), ("BsAs","Rosario",5.0)] ~?= False,
    "vuelos distintos con misma ruta y distinta duración" ~: vuelosValidos [("BsAs","Rosario",5.3), ("BsAs","Rosario",5.6)] ~?= False, 
    "vuelos con duración negativa" ~: vuelosValidos [("Madrid", "Barcelona", -1.5)] ~?= False,
    "vuelo válido mezclado con un inválido" ~: vuelosValidos [("BsAs", "Rosario", 5.0), ("Mendoza", "Jujuy", 0.0)] ~?= False
    ]

testsEjciudadesConectadas = test [    -- YA LO REVISE
    "ciudad conectada con un elemento (destino)" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0)] "Rosario" ~?= ["BsAs"],
    "ciudad conectada con un elemento (origen)" ~: ciudadesConectadas  [("BsAs", "Rosario", 5.0)] "BsAs" ~?= ["Rosario"],
    "ciudad conectada con múltiples conexiones" ~: expectPermutacion(ciudadesConectadas [("BsAs", "Rosario", 5.0), ("BsAs", "Córdoba", 4.0)] "BsAs") ["Rosario", "Córdoba"],
    "ciudad conectada sin vuelos" ~: ciudadesConectadas [] "Rosario" ~?= [],
    "ciudad no conectada" ~: ciudadesConectadas  [("BsAs","Tucumán",3.7)] "Salta" ~?= [],
    "ciudad conectada una vez" ~: ciudadesConectadas  [("BsAs","Tucumán",3.7),("Tucumán","Rosario",1.2)] "Rosario" ~?= ["Tucumán"],
    "ciudad conectada con un vuelo" ~: ciudadesConectadas [("Madrid", "Barcelona", 2.0), ("Barcelona", "Madrid", 3.0), ("Madrid", "Valencia", 1.5)] "Barcelona" ~?= ["Madrid"],
    "ciudad conectada desde ciudad"~: ciudadesConectadas  [("BsAs","Tucumán",2.7)] "BsAs" ~?= ["Tucumán"],
    "ciudades conectadas con 2 ciudades"~: expectPermutacion (ciudadesConectadas  [("BsAs", "Rosario", 5.0),("Rosario","BsAs",3.4),("BsAs","Jujuy",5.6),("Jujuy","Corrientes",2.3)] "BsAs") ["Rosario","Jujuy"]
    ]


testsEjmodernizarFlota = test [       -- YA LO REVISE
    "flota modernizada con un elemento" ~:  expectlistProximity (modernizarFlota [("BsAs", "Rosario", 10.0)]) [("BsAs", "Rosario", 9.0)],
    "flota modernizada con dos elemento" ~:  expectlistProximity(modernizarFlota [("BsAs", "Chubut", 10.0),("Chubut","Salta",9.8)]) [("BsAs", "Chubut", 9.0),("Chubut","Salta",8.82)],
    "no hay oferta de vuelos" ~: modernizarFlota [] ~?= [],
    "vuelos con duraciones muy bajas"~: expectlistProximity (modernizarFlota [("BsAs", "Rosario", 0.1)]) [("BsAs", "Rosario", 0.09)]
    ]


testsEjciudadMasConectada = test [ -- YA LO REVISE
    "ciudad mas conectada que aparece dos veces" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0)] ~?= "Rosario",
    "ciudad mas conectada con un solo vuelo" ~: expectAny (ciudadMasConectada [("BsAs", "Rosario", 10.0)]) ["BsAs","Rosario"],
    "caso empate" ~: expectAny (ciudadMasConectada  [("BsAs", "Rosario", 10.0), ("Rosario", "Cordoba", 7.0),("BsAs", "Salta", 7.0)]) ["BsAs","Rosario"],
    "caso varios empates" ~: expectAny ( ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Cordoba", 7.0),("BsAs", "Salta", 7.0),("BsAs", "Jujuy", 4.0),("Rosario", "Salta", 7.0),("Formosa", "Rosario", 9.0),("BsAs","Cordoba",5.4),("Cordoba","Salta",5.4),("Cordoba","Jujuy",6.3)]) ["BsAs","Rosario","Cordoba"],
    "ciudad muy conectada" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("BsAs", "Córdoba", 7.0), ("BsAs", "Mendoza", 12.0), ("Rosario", "Córdoba", 8.0)] ~?= "BsAs",
    "ciudad mas conectada con varios elementos" ~: expectAny ( ciudadMasConectada [("BsAs", "Rosario", 10.0),("Entre Rios","BsAs",3.5),("Santa Fe","Entre Rios",3.2)]) ["BsAs","Entre Rios"],
    "ciudad mas conectada con vuelos 'escala'" ~: expectAny (ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "Córdoba", 7.0), ("Córdoba", "Mendoza", 12.0),("Mendoza", "BsAs", 8.0)]) ["BsAs","Rosario","Córdoba","Mendoza"],
    "ciudad mas conectada con todos elementos distintos" ~: expectAny (ciudadMasConectada[("BsAs", "Rosario", 10.0), ("Córdoba", "Mendoza", 7.0),("Salta", "Jujuy", 5.0)]) ["BsAs","Rosario","Córdoba","Mendoza","Salta","Jujuy"],
    "ciudades con vuelos ida-vuelta (una sola conexion) (1)" ~: ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "BsAs", 10.0), ("Cordoba", "Entre Rios", 5.0), ("Cordoba", "Salta", 3.0)] ~?= "Cordoba",
    "ciudades con vuelos ida-vuelta (una sola conexion) (2)" ~: expectAny (ciudadMasConectada [("BsAs", "Rosario", 10.0), ("Rosario", "BsAs", 10.0)]) ["BsAs", "Rosario"]
    ]

testsEjsePuedeLlegar = test [ --YA LO REVISE
    "Se puede llegar caso verdadero con una escala" ~: sePuedeLlegar [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ~?= True,
    "Se puede llegar directo" ~: sePuedeLlegar [("Entre Rios", "Rosario", 5.0)] "Entre Rios" "Rosario" ~?= True,
    "No se puede llegar, destino y origen invertidos" ~: sePuedeLlegar [("Entre Rios", "Rosario", 5.0)] "Rosario" "Entre Rios" ~?= False,
   -- "Se puede llegar, una escala"  ~: sePuedeLlegar [("Rosario", "Entre Rios", 5.0),("Rosario","Montevideo",6.5),("Montevideo","Chaco",4.5)] "Rosario" "Chaco" ~?= True,
    "Se puede llegar, una escala, mismo destino y origen"  ~: sePuedeLlegar [("Entre Rios", "Rosario", 5.0),("Rosario","Entre Rios",6.5)] "Entre Rios" "Entre Rios" ~?= True,
    "No se puede llegar, mismo destino y origen, mas de una escala"  ~: sePuedeLlegar [("Entre Rios", "Rosario", 5.0),("Rosario","Montevideo",6.5),("Montevideo","Entre Rios",4.5)] "Entre Rios" "Entre Rios" ~?= False,
    "No se puede llegar, sin escalas"  ~: sePuedeLlegar [("Entre Rios", "Rosario", 5.0),("Montevideo","Chaco",4.5)] "Entre Rios" "Chaco" ~?= False,
    "No se puede llegar, mas de una escala" ~: sePuedeLlegar [("Entre Rios", "Rosario", 5.0),("Rosario","Montevideo",6.5),("Montevideo","BsAs",4.5)] "Entre Rios" "BsAs" ~?= False,
    "sin vuelos disponibles" ~: sePuedeLlegar [] "BsAs" "Córdoba" ~?= False
    ]

testsEjduracionDelCaminoMasRapido = test [
    "duración del camino más rápido con una escala" ~: expectAproximado (duracionDelCaminoMasRapido [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" "Córdoba" ) 10.0,
    "duración del camino más rápido sin escala" ~: expectAproximado (duracionDelCaminoMasRapido [("BsAs", "Rosario", 1.2)] "BsAs" "Rosario")  1.2,
    "duración del camino más rápido sin escala con vuelos en ambos sentidos " ~: expectAproximado (duracionDelCaminoMasRapido [("Corrientes", "Rosario", 4.3),("Rosario","Corrientes",4.2)] "Corrientes" "Rosario") 4.3,
    "duración del camino más rápido con una escala" ~: expectAproximado (duracionDelCaminoMasRapido [("Rosario","Jujuy", 5.0),("Santa Fe","BsAs",9.3),("Rosario","Salta",4.8)] "Rosario" "Salta") 4.8,
    "directo más rapida"  ~: expectAproximado( duracionDelCaminoMasRapido [("Rosario","Jujuy", 5.0),("Jujuy","BsAs",9.3),("Rosario","BsAs",10.2)] "Rosario" "BsAs" ) 10.2,
    "escala más rápido"  ~: expectAproximado(duracionDelCaminoMasRapido [("Rosario","Jujuy", 5.0),("Jujuy","BsAs",9.3),("Rosario","BsAs",15.6)] "Rosario" "BsAs" ) 14.3
    ]

testsEjpuedoVolverAOrigen = test [
    "puedo volver a origen con una escala" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "BsAs", 8.0)] "BsAs" ~?= True,
    "un vuelo solo que sale desde origen" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0)] "BsAs" ~?= False,
    "no hay oferta de vuelos" ~: puedoVolverAOrigen [] "BsAs" ~?= False,
    "puedo volver a origen con mas de una escala" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0), ("Rosario", "Córdoba", 5.0), ("Córdoba", "Asunción", 8.0),("Asunción","BsAs",6.7)] "BsAs" ~?= True,
    "no hay vuelos con destino a origen" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0),("BsAs","Corrientes",2.3)] "BsAs" ~?= False,
    "no hay escalas con destino a origen" ~: puedoVolverAOrigen [("BsAs", "Rosario", 5.0),("Rosario","Corrientes",4.5),("Santa Fe","BsAs",2.3)] "BsAs" ~?= False
    ]


-- Funciones extras

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [StringxStringxFloat], expected: [StringxStringxFloat]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para algun elemento t de actual existe un elemento x de expected tal que |t3-x3| < margenFloat

expectlistProximity:: [(String,String,Float)] -> [(String,String,Float)] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [(String,String,Float)] -> [(String,String,Float)] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoDeAUno actual expected)

esParecidoDeAUno :: [(String,String,Float)] -> [(String,String,Float)] -> Bool
esParecidoDeAUno [] _ = True
esParecidoDeAUno (x:xs) list2 = hayAproxEnLista x list2 && esParecidoDeAUno xs list2

hayAproxEnLista :: (String,String,Float) -> [(String,String,Float)] -> Bool
hayAproxEnLista _ [] = False

hayAproxEnLista (ac1,ac2,af) ((bc1,bc2,bf):xs) 
    | ac1 == bc1 && ac2 == bc2 = aproximado af bf
    |otherwise = hayAproxEnLista (ac1,ac2,af) xs

-- expectAproximado (actual: Float, expected: Float):Test
--asegura : res es un test verdadero si y sólo si:
--                  | actual - expected | < margenFloat

expectAproximado :: Float -> Float -> Test
expectAproximado actual expected = aproximado actual expected ~? ("expected approximation: " ++ show expected ++ "\nbut got: " ++ show actual)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat

-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)
---
