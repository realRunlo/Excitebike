-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g104 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(3,4,10),(2,10,-5),(10,2,30),(2,30,6),(20,10,-4),(1,1,1),(30,3,4),(2,15,-25),(3,9,39)] 

-- * Funções pré-definidas da Tarefa 1.

-- |Gera uma lista de número aleatórios
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.
-- |Gera o mapa
gera :: Int -> Int -> Int -> Mapa
gera 0 comprimento semente= [] 
gera npistas 1 semente = [Recta Terra 0]:gera (npistas -1) 1 semente
gera npistas comprimento semente = criapista (matriz (comprimento-1) p)
 where
 x=geraAleatorios (npistas*(comprimento-1)*2) semente  
 p=pares x

-- |Cria uma matriz
matriz :: Int -> [a] -> [[a]]
matriz n []=[]
matriz 0 xs = (take 1 xs : matriz 1 (drop 1 xs) )
matriz n xs =(take n xs : matriz n (drop n xs) )

-- |Forma pares
pares :: [Int] -> [(Int,Int)]
pares [] = []
pares (h:t:ts) | length (h:t:ts) /= 0 = ((h,t):pares ts)

-- |converte para piso
cpiso :: Piso->Int->Piso
cpiso ps x|x>=0 && x<=1 = Terra 
          |x>=2 && x<=3 =Relva
          |x==4 = Lama
          |x==5 = Boost
          |otherwise = ps
-- |Definição do tipo gama
type Gamas = (Int,Int)

-- |Cria a peca 
cpeca :: Gamas->[(Int,Int)]->[Peca]
cpeca (g1,g2) []= []
cpeca (g1,g2) ((x,y):t)|y>=0 && y<=1=(Rampa (cpiso (cgamaAnt g1) x) g2 (gamaAlt g2 y) ):cpeca (a,b) t
                       |y>=6 || (gamaAlt g2 y)==g2 = (Recta (cpiso (cgamaAnt g1) x) g2):cpeca (a,b) t
                       |y>=2 && y<=5 = (Rampa (cpiso (cgamaAnt g1) x) g2 (gamaAlt g2 y)):cpeca (a,b) t

 where
 a=cgamaAnt' g1 x  
 b=gamaAlt g2 y

-- |Devolve gama anterior ou o próprio 
cgamaAnt' :: Int->Int->Int
cgamaAnt' g1 x |x>=0 && x<=5 = x
               |otherwise = g1
                      
-- |Converte para piso dado um gama anterior                      
cgamaAnt :: Int->Piso
cgamaAnt g1 |g1>=0 && g1<=1 = Terra
            |g1>=2 && g1<=3 =Relva
            |g1==4 = Lama
            |otherwise = Boost
            
-- |Dado um gama calcula a altura
gamaAlt:: Int->Int-> Int
gamaAlt g2  y|y>=0 && y<=1 = g2+y+1
             |y>=2 && y<=5 && (g2-(y-1) )<0 = 0
             |y>=2 && y<=5 && (g2-(y-1) )==g2 = g2
             |y>=2 && y<=5 = (g2-(y-1))
             |otherwise = g2
 
-- |Cria a pista 
criapista ::[[(Int,Int)]] -> Mapa
criapista []=[]
criapista (((x,y):t):ts) = a:b

 where
 a= [Recta Terra 0]++(cpeca (0,0) ((x,y):t))
 b=(criapista ts)                                   

