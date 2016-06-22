-- DATOS Y SHOW

data Modificacion = Insertar Integer Char | Borrar Integer | Substituir Integer Char deriving (Show, Eq)

type PaqueteModificaciones = [Modificacion]

data Archivo = ArchivoVacio | NuevaVersion PaqueteModificaciones Archivo
instance Show Archivo where
    show ArchivoVacio = "Archivo vacio"
    show file = "Archivo: " ++ obtenerUltimaVersion file

data SCV = NuevoSCV | AgregarArchivo Archivo SCV
instance Show SCV where
    show NuevoSCV = "SCV vacio"
    show scv = verArchivos scv

verArchivos :: SCV -> String
verArchivos NuevoSCV = ""
verArchivos (AgregarArchivo file scv) = "- " ++ (show file) ++ "\n" ++ (verArchivos scv)

-- EJERCICIOS

-- Ejercicio 1/8
aplicarModificacion :: String -> Modificacion -> String
aplicarModificacion = error "Implementar!!! (ejercicio 1)"

-- Ejemplos:
-- Main> aplicarModificacion "d" (Insertar 1 'a')
-- "da"
-- Main> aplicarModificacion "d" (Insertar 0 'a')
-- "ad"
-- Main> aplicarModificacion "dato" (Borrar 1)
-- "ato"

-- Ejercicio 2/8
aplicarPaqueteModificaciones :: String -> PaqueteModificaciones -> String
aplicarPaqueteModificaciones = error "Implementar!!! (ejercicio 2)"

-- Ejemplos:
-- Main> aplicarPaqueteModificaciones "dato" [Substituir 1 'p', Insertar 4 's']
-- "patos"

-- Ejercicio 3/8
obtenerUltimaVersion :: Archivo -> String
obtenerUltimaVersion = error "Implementar!!! (ejercicio 3)"

-- Ejemplos: (ver def. archivo1 y archivo2 abajo)
-- Main> obtenerUltimaVersion archivo1
-- "dato"
-- Main> obtenerUltimaVersion archivo2
-- "ddato"

-- Ejercicio 4/8
cantVersiones :: Archivo -> Integer
cantVersiones = error "Implementar!!! (ejercicio 4)"

-- Ejemplos:
-- Main> cantVersiones archivo1
-- 1
-- Main> cantVersiones archivo2
-- 2

-- Ejercicio 5/8
obtenerVersion :: Integer -> Archivo -> String
obtenerVersion  = error "Implementar!!! (ejercicio 5)"

-- Ejemplos:
-- Main> obtenerVersion 1 archivo2
-- "dato"

-- Ejercicio 6/8
levenshtein :: String -> String -> Integer --PaqueteModificaciones
levenshtein = error "Implementar!!! (ejercicio 6)"

-- Ejemplos:
-- Main> levenshtein "auto" "automata"
-- 4

-- Ejercicio 7/8
levenshtein2 :: String -> String -> PaqueteModificaciones
levenshtein2 = error "Implementar!!! (ejercicio 7)"

-- Ejemplos:
-- Main> levenshtein2 "auto" "automata"
-- [Insertar 4 'a',Insertar 4 't',Insertar 4 'a',Insertar 4 'm']

-- Ejercicio 8/8
agregarVersion :: String -> Archivo -> Archivo
agregarVersion = error "Implementar!!! (ejercicio 8)"

-- Ejemplos:
-- Main> agregarVersion "dato" archivo2
-- Archivo: dato

-- Funciones provistas por la c\'atedra

len :: [a] -> Integer
len xs = fromIntegral (length xs)

-- Archivos

archivo1 = NuevaVersion [Insertar 0 'd', Insertar 1 'a', Insertar 2 't',Insertar 3 'o'] ArchivoVacio

archivo2 = NuevaVersion [Insertar 0 'd'] archivo1
