-- Trabajo Práctico: Sistema de Control de Versiones
-- 
-- Taller de Álgebra I
-- Primer cuatrimestre de 2016
-- 
-- Integrantes: Ignacio Andrés Reyna, Pablo xxxxx, Joaquin Vanderluis, Lorenzo Lamas
--
--
--********************************************************************************************************************
-- DATOS Y SHOW

data Modificacion = Insertar Integer Char | Borrar Integer | Substituir Integer Char deriving (Show, Eq)

type PaqueteModificaciones = [Modificacion]

data Archivo = ArchivoVacio | NuevaVersion PaqueteModificaciones Archivo
instance Show Archivo where
    show ArchivoVacio = "Archivo vacio"
    show file = "Archivo: " ++ obtenerUltimaVersion file


-- Funciones provistas por la c\'atedra

len :: [a] -> Integer
len xs = fromIntegral (length xs)

-- Archivos

archivo1 = NuevaVersion [Insertar 0 'd', Insertar 1 'a', Insertar 2 't',Insertar 3 'o'] ArchivoVacio

archivo2 = NuevaVersion [Insertar 0 'd'] archivo1

archivo3 = NuevaVersion [Insertar 0 'd'] archivo2

--********************************************************************************************************************
-- EJERCICIOS

-- Ejercicio 1/8


--Este es un auxiliar para poder ir guardando las letras que no precisen ser modificadas.
auxLetra = []

aplicarModificacion :: String -> Modificacion -> String

aplicarModificacion str1 (Insertar 0 letra2) = letra2:str1
--Caso base de Insertar
aplicarModificacion str1 (Borrar 0) = error "Debes elegir la posición del caracter a borrar."
aplicarModificacion (letra:str1) (Borrar 1) = str1
--Borrar aplicado con su excepcion y su caso base
aplicarModificacion str1 (Substituir 0 _) = error "Debes elegir la posición del caracter a substituir."
aplicarModificacion (caracter1:str1) (Substituir 1 caracter2) = (caracter2:str1) 
--Substituir aplicado con su excepcion y su caso base
aplicarModificacion (letra1:str1) (Insertar posicion letra2) |posicion>((len str1)+1) = error "La posición elegida excede la cantidad de letras en la palabra."
                                                             |otherwise= (letra1:auxLetra) ++ aplicarModificacion str1 (Insertar (posicion-1) letra2)
aplicarModificacion (letra1:str1) (Borrar posicion) | posicion>((len str1)+1) = error "La posición elegida excede la cantidad de letras en la palabra."
                                                    | otherwise = (letra1:auxLetra) ++ aplicarModificacion str1 (Borrar (posicion-1))
aplicarModificacion (letra1:str1) (Substituir posicion letra2) | posicion>((len str1)+1) = error "La posición elegida excede la cantidad de letras en la palabra."
                                                               | otherwise = (letra1:auxLetra) ++ aplicarModificacion str1 (Substituir (posicion-1) letra2)


--Nosotros lo pensamos defininendo una nueva lista que mantenga sin cambios lo que asi debe mantenerse y que luego lo una a la palabra reversionada.

-- Ejercicio 2/8
aplicarPaqueteModificaciones :: String -> PaqueteModificaciones -> String
aplicarPaqueteModificaciones str1 [instruccion1] = aplicarModificacion str1 instruccion1
aplicarPaqueteModificaciones str1 (instruccion1:listaDeInstrucciones) = aplicarPaqueteModificaciones (aplicarModificacion str1 instruccion1) listaDeInstrucciones

-- Ejercicio 3/8
obtenerUltimaVersion :: Archivo -> String
obtenerUltimaVersion ArchivoVacio = []
obtenerUltimaVersion (NuevaVersion paquetedeDeModificaciones1 archivoVariable) = aplicarPaqueteModificaciones (obtenerUltimaVersion archivoVariable) paquetedeDeModificaciones1
--Esta función la pensamos haciendo que baje por el Arbol hasta la versión primera, y luego vaya aplicando todos los paquetes de modificaciones correspondientes.

-- Ejercicio 4/8
cantVersiones :: Archivo -> Integer
cantVersiones ArchivoVacio = 0
cantVersiones (NuevaVersion paquetedeDeModificaciones1 archivoVariable) = 1 + (cantVersiones archivoVariable)

-- Ejercicio 5/8
obtenerVersion :: Integer -> Archivo -> String
obtenerVersion numero ArchivoVacio | numero == 1 = []
                                   | numero > 1 = error "El número de versión no puede exceder la cantidad de versiones."
--En este caso pensamos la recursión fijandonos si la versión pedida era la ultima versión, caso contrario, que fuera bajando un nivel por el Arbol hasta llegar a la version solicitada.
obtenerVersion numero (NuevaVersion paquetedeDeModificaciones1 archivoVariable) | numero > cantVersiones (NuevaVersion paquetedeDeModificaciones1 archivoVariable) = error "El número de versión no puede exceder la cantidad de versiones."
                                                                                | numero == cantVersiones (NuevaVersion paquetedeDeModificaciones1 archivoVariable) = obtenerUltimaVersion (NuevaVersion paquetedeDeModificaciones1 archivoVariable)                                                                                
                                                                                | otherwise = obtenerVersion numero archivoVariable


-- Ejercicio 6/8
--Función auxiliar que devuelve el mínimo entre tres Integers.
minimo :: Integer -> Integer -> Integer -> Integer
minimo n1 n2 n3 | n1 <= n2 && n1 <= n3 = n1
                | n2 < n1 && n2 <= n3 = n2
                | n3 < n1 && n3 < n2 = n3

levenshtein :: String -> String -> Integer --PaqueteModificaciones
levenshtein [] str2 = (len str2)
levenshtein str1 [] = (len str1)
--Ahora vamos a dividir entre los casos en los que hay que sumar 1 al Levenshtein (init str1) (init str2) y los que no.
levenshtein str1 str2 | last str1 /= last str2 = minimo  ((levenshtein (str1) (init (str2)))+1) ((levenshtein (init (str1)) (str2))+1) ((levenshtein (init (str1)) (init (str2)))+1) 
                      | otherwise = minimo ((levenshtein (str1) (init (str2)))+1) ((levenshtein (init (str1)) (str2))+1) (levenshtein (init (str1)) (init (str2))) 


-- Ejercicio 7/8
--Función auxiliar que devuelve la lista de menor longitud.
minLength :: [a] -> [a] -> [a] -> [a]
minLength l1 l2 l3 | len(l1) <= len(l2) && len(l1) <= len(l3) = (l1)
                   | len(l2) < len(l1) && len(l2) <= len(l3) = (l2)
                   | len(l3) < len(l1) && len(l3) < len(l2) = (l3)


levenshtein2 :: String -> String -> PaqueteModificaciones
levenshtein2 [] [] = []
levenshtein2 [] str2 = [Insertar 0 (last str2)] ++ levenshtein2 [] (init str2)
levenshtein2 str1 [] = [Borrar 1] ++ levenshtein2 (tail str1) []
--En esta ocasión decidimos dividr entre los casos y obtener la lista mas corta posible.
levenshtein2 str1 str2 |(last str1) == (last str2) = minLength ((Borrar (len str1)):(levenshtein2 (init(str1)) str2)) ((Insertar (len(str1)) (last(str2))) : (levenshtein2 str1 (init(str2)))) (levenshtein2 (init(str1)) (init(str2)))
                       | otherwise = minLength ((Borrar(len(str1))) : (levenshtein2 (init(str1)) str2)) (((Insertar (len (str1)) (last(str2)))) : (levenshtein2 str1 (init(str2)))) ((Substituir (len(str1)) (last(str2))) : (levenshtein2 (init(str1)) (init(str2))))


-- Ejercicio 8/8
agregarVersion :: String -> Archivo -> Archivo
agregarVersion str1 file | str1 == (obtenerUltimaVersion file) = error "No estas aplicando ninguna modificación."
                         |otherwise = NuevaVersion (levenshtein2 (obtenerUltimaVersion file) str1) file

