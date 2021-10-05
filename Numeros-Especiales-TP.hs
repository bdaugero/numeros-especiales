--Ejercicio 1.
esSumaDeDosCubos :: Integer -> Bool
esSumaDeDosCubos n | sumaCubos n (parteEnteraDeRaiz3 n) 1 > 0 = True
                   | otherwise = False
--Ejericicio 2.                   
descomposicionCubos :: Integer -> (Integer,Integer)
descomposicionCubos n = sumaCubosTupla n (parteEnteraDeRaiz3 n) 1

--Ejericicio 3.
cantidadDeFormas :: Integer -> Integer
cantidadDeFormas  n | even(sumaCubos n (parteEnteraDeRaiz3 n) 1)  = div (sumaCubos n (parteEnteraDeRaiz3 n) 1) 2  --Divide por 2 porque le llegan los resultados duplicados((2 3)(3 2)).
                    | otherwise = 1 + div (sumaCubos n (parteEnteraDeRaiz3 n) 1) 2 

--Funcion que dado un n devuelve la parte entera de su raiz cubica.
parteEnteraDeRaiz3 :: Integer -> Integer
parteEnteraDeRaiz3 n = floor (fromIntegral(n) ** (1/3))

--Funcion auxiliar de cantidadDeFormas y de esSumaDeDosCubos
sumaCubos :: Integer -> Integer -> Integer -> Integer
sumaCubos numOriginal primerSumando segundoSumando | segundoSumando^3 > numOriginal = 0
                                                   | suma == numOriginal = 1 + sumaCubos numOriginal (primerSumando - 1) segundoSumando
                                                   | suma > numOriginal = sumaCubos numOriginal (primerSumando - 1) segundoSumando
                                                   | suma < numOriginal = sumaCubos numOriginal primerSumando (segundoSumando + 1)
  where suma = primerSumando^3 + segundoSumando^3

--Funcion auxiliar de descomposicionCubos
sumaCubosTupla :: Integer -> Integer -> Integer -> (Integer, Integer)
sumaCubosTupla numOriginal primerSumando segundoSumando | segundoSumando^3 > numOriginal = (0,0)
                                                        | suma == numOriginal = (segundoSumando, primerSumando)
                                                        | suma > numOriginal = sumaCubosTupla numOriginal (primerSumando - 1) segundoSumando
                                                        | suma < numOriginal = sumaCubosTupla numOriginal primerSumando (segundoSumando + 1)
  where suma = primerSumando^3 + segundoSumando^3

--Ejericicio 4.
--Recursion de numeros especiales. Dame el proximo especial mayor a n.
especialDesde :: Integer -> Integer
especialDesde n | cantidadDeFormas n > 1 = n
                | otherwise = especialDesde (n+1)

--Ejericicio 5.
especialNumero :: Integer -> Integer
especialNumero n = especialNumeroAux n 0 1

--Funcion que recibe el orden del numero especial un numero auxiliar y un numero en el cual va a guardar el numero actual.
especialNumeroAux :: Integer  -> Integer -> Integer -> Integer
especialNumeroAux orden ind n | orden == ind = (n-1)
                              | cantidadDeFormas n > 1 = especialNumeroAux orden (ind+1) (n+1)
                              | otherwise = especialNumeroAux orden ind (n+1)

--Ejercicio 6.
esMuyEspecial :: Integer -> Bool
esMuyEspecial n = esMuyEspecialAux n (parteEnteraDeRaiz3 n)

esMuyEspecialAux n k | k < 1 = False
                     | k == 1 && (esEspecial n) = True
                     | not(esUnCubo k) = esMuyEspecialAux n (k-1)
                     | mod n k == 0 && esEspecial(div n k) = False 
                     | mod n k /= 0 || not(esEspecial(div n k)) = esMuyEspecialAux n (k-1)

esEspecial :: Integer -> Bool
esEspecial n = cantidadDeFormas n > 1
                     
esUnCubo :: Integer -> Bool
esUnCubo x = (round (fromIntegral x**(1/3)))^3 == x                     
