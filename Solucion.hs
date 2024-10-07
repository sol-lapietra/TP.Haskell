module Solucion where 

{--
Nombre del grupo: Askell
Integrantes: 
- Sol La Pietra: 46346822, lapietrasol@gmail.com
- Almendra Gandini: 44668798, almendragandini1234@gmail.com
- Ariana Abril Medina: 45429427, ariabril.med773@gmail.com
--}

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]

-- EJERCICIO 1
vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True
vuelosValidos (v:vs) | not (vueloValido v) = False
                     | pertenece v vs      = False
                     | otherwise           = vuelosValidos vs

vueloValido :: Vuelo -> Bool
vueloValido (c1, c2, t) = (c1 /= c2) && (t > 0.00)

pertenece :: Vuelo -> AgenciaDeViajes -> Bool
pertenece _ [] = False
pertenece (v11, v12, t) ((v21, v22, t1):xs) = (v11 == v21 && v12 == v22) || pertenece (v11, v12, t) xs  


-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas agencia ciudad = eliminarRepetidos (buscarCiudadesDesdeYHacia agencia ciudad)

-- Función para buscar ciudades conectadas desde y hacia la ciudad
buscarCiudadesDesdeYHacia :: AgenciaDeViajes -> Ciudad -> [Ciudad]
buscarCiudadesDesdeYHacia [] _ = []
buscarCiudadesDesdeYHacia ((v11, v12, _):xs) ciudad
  | v11 == ciudad = v12 : buscarCiudadesDesdeYHacia xs ciudad  -- Ciudad a ciudad de destino
  | v12 == ciudad = v11 : buscarCiudadesDesdeYHacia xs ciudad  -- Ciudad de origen a ciudad
  | otherwise = buscarCiudadesDesdeYHacia xs ciudad

eliminarRepetidos :: [Ciudad] -> [Ciudad] 
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitar x xs)

quitar :: Ciudad -> [Ciudad] -> [Ciudad]
quitar _ [] = []
quitar ciudad (x:xs) | ciudad == x = quitar ciudad xs
                     | otherwise = x : quitar ciudad xs


-- EJERCICIO 3

modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota ((origen, destino, duracion):xs) = (origen, destino, duracion * 0.9) : modernizarFlota xs


-- EJERCICIO 4
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada agencia = ciudadConMasConexiones agencia (obtenerCiudades agencia) ""

obtenerCiudades :: AgenciaDeViajes -> [Ciudad]
obtenerCiudades [] = []
obtenerCiudades ((v1, v2, _):xs) = eliminarRepetidos [v1, v2] ++ obtenerCiudades xs

contarConexiones :: AgenciaDeViajes -> Ciudad -> Int
contarConexiones agencia ciudad = longitud (ciudadesConectadas agencia ciudad)

ciudadConMasConexiones :: AgenciaDeViajes -> [Ciudad] -> Ciudad -> Ciudad
ciudadConMasConexiones _ [] ciudadMax = ciudadMax
ciudadConMasConexiones agencia (ciudad:ciudades) ciudadMax
  | contarConexiones agencia ciudad > contarConexiones agencia ciudadMax = ciudadConMasConexiones agencia ciudades ciudad
  | otherwise = ciudadConMasConexiones agencia ciudades ciudadMax

longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs


-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False
sePuedeLlegar rutas origen destino = hayVueloDirecto origen destino rutas || hayVueloConEscala origen destino rutas

hayVueloDirecto :: Ciudad -> Ciudad -> AgenciaDeViajes -> Bool
hayVueloDirecto _ _ [] = False
hayVueloDirecto origen destino ((c,t,m):cs) | origen == c && destino == t = True  
                                            | otherwise = hayVueloDirecto origen destino cs

hayVueloConEscala :: Ciudad -> Ciudad -> AgenciaDeViajes -> Bool
hayVueloConEscala a b [] = False 
hayVueloConEscala a b ((x, y, _):xs) = (x == a && hayVueloDirecto y b xs) || hayVueloConEscala a b xs


-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido [(v11, v12, t)] c1 c2 | v11 == c1 && v12 == c2 = t  -- vuelo directo
duracionDelCaminoMasRapido ((v11, v12, t):(v21, v22, t1):xs) c1 c2 | v11 == c1 && v12 == c2 = t  
                                                                   | v11 == c1 && v12 == v21 && v22 == c2 = menor (t + t1) (duracionDelCaminoMasRapido ((v21, v22, t1):xs) c1 c2)
                                                                   | otherwise = duracionDelCaminoMasRapido ((v21, v22, t1):xs) c1 c2
menor :: Duracion -> Duracion -> Duracion
menor x y | x < y = x
          | otherwise = y


-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad -> Bool
puedoVolverAOrigen agencia origen = buscarRuta agencia origen origen

buscarRuta :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
buscarRuta [] _ _ = False
buscarRuta ((v11, v12, _):xs) origen destino | v11 == destino && v12 == origen = True
                                             | v11 == destino = buscarRuta xs origen v12
                                             | v12 == origen = buscarRuta xs origen v11
                                             | otherwise = buscarRuta xs origen destino


