import Text.Show.Functions ()

length' :: [a] -> Int
length' []        = 0                   -- caso base
length' (_:xs) = 1 + length' xs 


n :: [Int]
n = [1,2,3]
{--
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acumulador []     =  acumulador
foldr operador acumulador (cabeza : cola) = cabeza `operador` foldr operador acumulador cola --}


b :: [Int]
b = [2,3]

c :: [Int]
c = [3]

--Definir la función esMultiploDeAlguno/2, que recibe un número y una lista
 --y devuelve True si el número es múltiplo de alguno de los números de la lista.
unoesmultide :: Int -> [Int] -> Bool
unoesmultide numero list = any (multide numero) list

multide :: Int -> Int -> Bool
multide numero otronumero = mod otronumero numero == 0


--Armar una función promedios/1, que dada una lista de 
--listas me devuelve la lista de los promedios de cada lista-elemento.

promedios :: [[Float]] ->[Float]
promedios listas = map promediolista listas

promediolista :: [Float] -> Float
promediolista lista 
 | leng lista == 0 = 0
 | otherwise = sum lista / leng lista


leng :: Num a => [a] -> Float
leng [] = 0
leng (_:xs) = 1 + leng xs

{--Armar una función promediosSinAplazos que dada una lista de listas 
me devuelve la lista de los promedios de cada lista-elemento, excluyendo 
los que sean menores a 4 que no se cuentan. P.ej.--}

prueba :: [[Float]]
prueba =  [[1,2,3,4],[4,4,4],[10,10,10,5,5],[5],[0],[]]

promsinAplazos :: [[Float]] -> [Float]
promsinAplazos listas = filter esmayorigual4 (map prom listas)

esmayorigual4 :: Float -> Bool
esmayorigual4 numero = numero >= 4
prom :: [Float] -> Float
prom lista 
 | leng lista == 0 = 0
 | otherwise = sum lista / leng lista


{--Definir la función aprobó/1, que dada la lista de las notas de 
un alumno devuelve True si el alumno aprobó. Se dice que un alumno 
aprobó si todas sus notas son 6 o más--}

aprobo :: [Float] -> Bool
aprobo notas = minimum notas >= 6
{-- 17.
1 año : 22 cm
así bajando de a 2 cm por año hasta 
9 años: 6 cm 
10 a 15 años: 4 cm 
16 y 17 años: 2 cm 
18 y 19 años: 1 cm 
20 años o más: 0 cm 
Definir la función crecimientoAnual/1,que recibe como parámetro la edad de la persona, 
y devuelve cuánto tiene que crecer en un año. Hacerlo con guardas. 
La fórmula para 1 a 10 años es 24 - (edad * 2). --}

type Edad = Int
type Crecimiento = Int
crecimientoanual :: Edad -> Crecimiento
crecimientoanual edad 
 | edad <=10  = 24 - (edad * 2)
 | edad >10 && edad <= 15 = 4
 | edad >15 && edad <=17 = 2
 | edad >17 && edad <=19 = 1
 | otherwise = 0