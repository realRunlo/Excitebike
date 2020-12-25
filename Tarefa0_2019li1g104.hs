-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g104 where

-- * Funções não-recursivas.

import Data.Fixed -- usei para poder usar mod' pq o 'mod' não estava a funcionar

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving Show

-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.
toRadianos :: Double->Double  -- converte graus para radianos ( as funções em haskell só percebem radianos)
toRadianos a = (pi*a)/180


toCartesiano :: Vetor -> Vetor
toCartesiano (Cartesiano x y ) = (Cartesiano x y )    
toCartesiano (Polar r t) = (Cartesiano x y )                                                                 
  where 
  x=r*cos t'
  y=r*sin t' 
  t'=toRadianos t

toPolar :: Vetor -> Vetor
toPolar (Cartesiano x y) = (Polar r t)            --converte um vetor cartesiano para polar
 where
 r = sqrt(x^2 + y^2)
 t = tan(y/x)

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) =(Cartesiano (x1+x2) (y1+y2) )
somaVetores a b = somaVetores a' b'
 where 
 a' = toCartesiano a
 b' = toCartesiano b

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = (Cartesiano (x1-x2) (y1-y2) )
subtraiVetores a b = subtraiVetores a' b'
 where 
 a'= toCartesiano a
 b'= toCartesiano b

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor a (Cartesiano x y) = (Cartesiano (a*x) (a*y))
multiplicaVetor a (Polar r t) = multiplicaVetor a pl
 where
 pl= toCartesiano (Polar r t)

-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = if t1>=0 && t1<=1 && t2>=0 && t2<=1 then True else False 
 where
 t1=((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3)) / ((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
 t2=((y1-y2)*(x1-x3)+(x2-x1)*(y1-y3)) / ((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3)) 
intersetam (a,b) (c,d) = intersetam (a',b') (c',d')                                                           
 where 
 a'= toCartesiano a
 b'= toCartesiano b  
 c'= toCartesiano c
 d'= toCartesiano d              
  
-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = somaVetores (Cartesiano x1 y1)  (Cartesiano (t*(x2-x1)) (t*(y2-y1)))
 where
 t=((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3) )
intersecao (a,b) (c,d)= intersecao  (a',b') (c',d')                                                            
 where
 a'= toCartesiano a
 b'= toCartesiano b  
 c'= toCartesiano c
 d'= toCartesiano d  
                                                                              
   

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x [] = False
eIndiceListaValido x a = if x >=0 && x<= ((length a) -1 )then True    --ok
                              else False

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensaoMatriz ([]:s) = (0,0)
dimensaoMatriz ((h:t):ts) = (linhas,colunas)
                   
 where
 linhas=length ((h:t):ts)
 colunas=length (h:t)

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida _ [] = False
ePosicaoMatrizValida _ ([]:t) = False
ePosicaoMatrizValida (l,c) ((h:t):ts) = if l>=0 && l<=linha && c>=0 && c<=coluna then True  --ok
                                             else False
            
  where
  linha=length ((h:t):ts) -1
  coluna=length (h:t) -1
    

-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo a |a==360 = 0
normalizaAngulo a | a<0 = if a>(-360) then 360+a else 360+(mod' a (-360))
normalizaAngulo a | a<360 = a
normalizaAngulo a | a>360 = mod' a 360
-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a                                                      --numereção dos elementos das listas está a começar no 0
encontraIndiceLista x (h:t)| x<=((length (h:t)) -1) && x>=0  = if x==0 then h
                                                   else encontraIndiceLista (x-1) t
       
       
-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista x a (h:t) = if x==0 then (a:t)                                          --ok
                                else h:atualizaIndiceLista (x-1) a t

-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a 
encontraPosicaoMatriz (l,c) ((h:t):ts) = if l==0 then encontraIndiceLista c (h:t)         --posição (0,0) corresponde ao primeiro elemento da matriz
                                            else encontraPosicaoMatriz  (l-1,c) ts

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) a ((h:t):ts) = if l==0 then ( (atualizaIndiceLista  c a (h:t)  ):ts )     
                                            else ((h:t):atualizaPosicaoMatriz  (l-1,c) a ts)            


