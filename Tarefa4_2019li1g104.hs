-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g104 where

import LI11920
import Tarefa0_2019li1g104
import Tarefa2_2019li1g104

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(0.5,mapa1,Jogador 0 1.1 0.1 5 (Chao True)),
             (1,mapa1,Jogador 1 2 2 5 (Chao True)),
              (0.5,mapa1,Jogador 2 2.1 0.6 4 (Ar 2 (30) 0)),
               (1,mapa1,Jogador 3 2.1 2 4 (Ar 2 (-30) 0)),
                (1,mapa1,Jogador 4 2.9 2 4 (Chao False)),
                 (1,mapa1,Jogador 5 2.5 2 3 (Ar 1.5 70 1)),
                  (1,mapa1,Jogador 6 3.2 2 3 (Ar 1.5 80 1)),
                   (0.5,mapa1,Jogador 7 3.2 0 3 (Morto 1)),
                    (1,mapa1,Jogador 8 2.1 0.8 3 (Ar 1.15 (30) 0)),
                     (1,mapa1,Jogador 9 2.8 1.5 3 (Chao True)),
                      (1.5,mapa1,Jogador 10 3.2 0 3 (Morto 1)),
                       (0.5,mapa1,Jogador 11 1.1 0.2 5 (Chao True)),
                        (0.5,mapa1,Jogador 12 1.1 0.2 5 (Chao True)),
                         (0.5,mapa1,Jogador 13 1.1 0.2 5 (Chao True)),
                          (1,mapa1,Jogador 14 2.1 1.2 3 (Ar 1.15 (70) 0)),
                           (0.5,mapa1,Jogador 15 2 0.3 5 (Chao True)),
                            (2,mapa1,Jogador 16 2.9 1.2 3 (Ar 0.6 (70) 0)),
                             (2,mapa1,Jogador 17 2.2 1.2 3 (Ar 1.1 (87) 0)),
                              (0.6,mapa1,Jogador 18 2.9 1 3 (Ar 1.1 (87) 0)),
                               (2,mapa1,Jogador 19 2.8 1 3 (Ar 1.3 (30) 0)),
                                (1,mapa1,Jogador 20 2 2.1 3 (Ar 1 (86) 0))]
 
mapa1=[[Recta Terra 0,Recta Terra 0,Rampa Boost 0 1,Rampa Terra 1 0],
        [Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 1],
         [Recta Terra 0,Rampa Boost 0 1,Rampa Boost 1 0,Recta Lama 0],
          [Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Terra 0],
           [Recta Terra 0,Recta Lama 0 , Recta Relva 0 ,Rampa Terra 0 1],
            [Recta Terra 0,Rampa Terra 0 1, Rampa Terra 1 0 ,Recta Terra 0],
             [Recta Terra 0,Rampa Terra 0 1, Rampa Terra 1 0,Recta Terra 0],
              [Recta Terra 0,Rampa Terra 0 1, Rampa Terra 1 0,Recta Terra 0],
               [Recta Terra 0,Rampa Boost 0 1, Recta Relva 1 ,Rampa Terra 1 0],
                [Recta Terra 0,Rampa Relva 0 1, Recta Relva 1 ,Rampa Terra 1 0],
                 [Recta Terra 0,Rampa Boost 0 1,Rampa Boost 1 0,Recta Relva 0],
                  [Recta Terra 0,Recta Lama 0,Rampa Boost 0 1,Rampa Terra 1 0],
                   [Recta Terra 0,Recta Boost 0,Rampa Boost 0 1,Rampa Terra 1 0],
                    [Recta Terra 0,Recta Cola 0,Rampa Boost 0 1,Rampa Terra 1 0],
                     [Recta Terra 0,Rampa Boost 0 1, Recta Relva 1 ,Rampa Terra 1 0],
                      [Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Rampa Terra 1 0],
                       [Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Terra 0],
                        [Recta Terra 0,Rampa Relva 0 1,Recta Relva 1 ,Recta Terra 1],
                         [Recta Terra 0,Rampa Relva 0 1,Recta Relva 1 ,Recta Terra 1],
                          [Recta Terra 0,Rampa Relva 0 1,Recta Lama 1 ,Recta Terra 1],
                           [Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0 ,Recta Terra 0]]
 

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera tempo mapa (Jogador pj dj vj cj (Morto x))  = (Jogador pj dj vj cj (Morto x))
acelera tempo mapa (Jogador pj dj vj cj (Chao acj)) =  (Jogador pj dj (ncVJ)  cj (Chao acj))
 where
 jC=(Jogador pj dj vj cj (Chao acj))
 atrito = getAtrito mapa jC
 accel = getAccel jC
 ncVJ = nVelocidadeC tempo atrito accel jC
acelera tempo mapa (Jogador pj dj vj cj (Ar alt inc g)) = (Jogador pj dj (naVJ) cj (Ar alt inc (ng)))                                           
 where
 jA=(Jogador pj dj vj cj (Ar alt inc g))
 naVJ = nVelocidadeA tempo jA
 ng = nGravidade tempo jA

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move tempo mapa j|estaMorto j = descontaTimeout tempo j
                 |estaNochao j = moveChao tempo mapa j
                 |otherwise = moveAr tempo mapa j
                                        

 
-- |Obtem gravidade de um jogador
getGrav :: Jogador ->Double
getGrav (Jogador _ _ _ _ (Ar _ _ gi))  = gi

-- |Calcula o novo tempo para quando se atinge o limite de uma peca
novoTempo :: Double -> Double -> Double -> Double
novoTempo inclinacao velocidade dpercorrer = dpercorrer/((cos (toRadianos(inclinacao))) * velocidade)
  

-- |Calcula a velociade quando o jogador se encontra no chão
nVelocidadeC :: Double -> Double -> Int -> Jogador -> Double
nVelocidadeC tempo atrito accelM (Jogador _ _ vj _ _) |vfinal<0 = 0
                                                      |otherwise = vfinal
 where
 vfinal=vj+((fromIntegral accelM)-atrito*vj)*tempo

-- |Calcula a velociade quando o jogador se encontra no ar
nVelocidadeA ::Double -> Jogador -> Double
nVelocidadeA tempo (Jogador _ _ vj _ _) |vj<0 = 0
                                        |otherwise = vfinal


 where
 vfinal = vj-(0.125*vj*tempo)

-- |Calcula a gravide do jogador no ar
nGravidade :: Double -> Jogador -> Double
nGravidade tempo (Jogador _ _ _ _ (Ar _ _ gi)) = gi + 1*tempo

-- |Devolve o atrito do piso em que o jogador se encontra
getAtrito :: Mapa -> Jogador -> Double
getAtrito mapa (Jogador pj dj vj cj ej) |pisoJ==Terra = 0.25
                                        |pisoJ==Relva = 0.75
                                        |pisoJ==Lama  = 1.5
                                        |pisoJ==Boost = -0.5
                                        |otherwise    = 3.00
 where
 pisoJ = getPiso (encontraPosicaoMatriz (pj,floor dj) mapa)

-- |Devolve o piso de uma peca
getPiso :: Peca -> Piso
getPiso (Recta piso x) = piso
getPiso (Rampa piso x y) = piso

-- |Devolve aceleração mota
getAccel :: Jogador -> Int
getAccel (Jogador pj dj vj cj (Chao a)) |vj<2 && a = 1
                                        |otherwise = 0



-- |Atualiza o tempo de timeout e verifica se este acabou atualizando em conformidade
descontaTimeout :: Double -> Jogador -> Jogador
descontaTimeout tempo (Jogador pj dj vj cj (Morto to)) |nT>0 = (Jogador pj dj vj cj (Morto nT ))
                                                       |otherwise = (Jogador pj dj (0) cj (Chao False ))
 where
 nT=to-tempo

-- |Testa se  limite da peca foi atingido

testaLimPec ::  Double -> Jogador -> Bool
testaLimPec  tempo (Jogador pj dj vj cj ej) = if (estaNochao j) && (distanciaTotal1>=1) then True 
                                                  else if not (estaNochao j) && (distanciaTotal2>1) then True 
                                                     else False
 where
 j  = (Jogador pj dj vj cj ej) 
 dC = vj * tempo
 distanciaTotal1 = (dj-(fromIntegral (floor dj))) + dC 
 inc = ecliEjGet j
 dcA = (cos (abs (toRadianos inc)) ) * vj * tempo
 distanciaTotal2 = (dj-(fromIntegral (floor dj))) + dcA                                                


-- |Calcula a inclinação de uma dada peça
inclinacaoPeca :: Peca-> Double
inclinacaoPeca (Recta _ x ) = 0
inclinacaoPeca (Rampa _ x y) | y>x = toGraus (atan (fromIntegral (y)))
inclinacaoPeca (Rampa _ x y) | y<x = - toGraus (atan (fromIntegral (x)))

-- |Testa se a inclinação de embate é mortal ou não
inclinacaoM:: Mapa -> Jogador -> Bool
inclinacaoM mapa (Jogador pj dj vj cj (Ar alt inc g)) |(inc-incPecaJ)>=45 = True
                                                      |otherwise = False

 where
 incPecaJ=inclinacaoPeca (encontraPosicaoMatriz (pj,floor dj) mapa)



-- |Calcula os efeitos da passagem do tempo no ar
moveChao :: Double -> Mapa -> Jogador -> Jogador
moveChao tempo mapa (Jogador pj dj vj cj ej) |not (testaLimPec tempo jogador) =  jogador {distanciaJogador =  (dj+distanciaPercorridaC) }
                                             |(testaLimPec tempo jogador) && (inclinacaoPecAtual<=inclinacaoPecSeguinte) = jogador {distanciaJogador = fromIntegral((floor dj)+1)}
                                             |(testaLimPec  tempo jogador) && (inclinacaoPecAtual>inclinacaoPecSeguinte) = jogador {estadoJogador = Ar alturaFpeca (inclinacaoPecAtual) 0,distanciaJogador = fromIntegral((floor dj)+1)}
 where
 jogador = (Jogador pj dj vj cj ej)
 distanciaPercorridaC = (cos (abs(toRadianos inclinacaoPecAtual))) * vj * tempo
 inclinacaoPecAtual = inclinacaoPeca (encontraPosicaoMatriz (pj,floor dj) mapa)
 inclinacaoPecSeguinte = inclinacaoPeca (encontraPosicaoMatriz (pj,floor (dj+1)) mapa)
 alturaFpeca = getAltFPec (encontraPosicaoMatriz (pj,floor dj) mapa)

-- |Calcula os efeitos da passagem do tempo no ar
moveAr :: Double -> Mapa -> Jogador -> Jogador
moveAr tempo mapa (Jogador pj dj vj cj ej) |dj== fromIntegral (floor dj) = j {estadoJogador = (Ar (y) incJ grav),distanciaJogador =  (dj+x) }
                                           |not (testaLimPec tempo j) && not (intersetam  (posAtual,posFinal) (p1,p2))  =  j {estadoJogador = (Ar (y) incJ grav),distanciaJogador =  (dj+x) }
                                           |not (testaLimPec tempo j) && (intersetam  (posAtual,posFinal) (p1,p2)) && not (inclinacaoM mapa j)  = j {estadoJogador = Chao False,distanciaJogador =((fromIntegral (floor dj))+a),velocidadeJogador=nv }
                                           |not (testaLimPec tempo j) && (intersetam  (posAtual,posFinal) (p1,p2)) && (inclinacaoM mapa j) = j {estadoJogador = Morto 1,distanciaJogador =((fromIntegral (floor dj))+a),velocidadeJogador=0}
                                           |(testaLimPec tempo j) && not (intersetam  (posAtual,posFinal) (p1,p2)) = j {estadoJogador = (Ar (y2) incJ grav),distanciaJogador = fromIntegral((floor dj)+1) }
                                           |(testaLimPec tempo j) && (intersetam  (posAtual,posFinal) (p1,p2)) && not (inclinacaoM mapa j) = j {estadoJogador = Chao False,distanciaJogador = fromIntegral((floor dj)+1),velocidadeJogador=nv }
                                           |(testaLimPec tempo j) && (intersetam  (posAtual,posFinal) (p1,p2)) && (inclinacaoM mapa j) = j {estadoJogador = Morto 1,distanciaJogador = fromIntegral((floor dj)+1),velocidadeJogador=0 }
                          

 where
 j     = (Jogador pj dj vj cj ej)
 pecaJ = encontraPosicaoMatriz (pj,floor dj) mapa
 yPeca = getAltFPec pecaJ
 incJ  = ecliEjGet j
 altJ  = altEjGet j 
 grav  = getGrav j
 nt    = novoTempo incJ vj (fromIntegral(ceiling dj)-dj)
 p1    = ponto1 pecaJ
 p2    = ponto2 pecaJ
 posAtual = Cartesiano (dj-(fromIntegral (floor dj))) altJ
 posFinal = Cartesiano ((dj-(fromIntegral (floor dj))) + (cos (toRadianos incJ))*vj*tempo) (altJ + (sin (toRadianos incJ) * vj * tempo ) - grav*tempo ) 
 (Cartesiano a b) = intersecao (posAtual,posFinal) (p1,p2)
 vetorG  = (Cartesiano 0 (-grav*tempo) )
 vetorG2 = (Cartesiano 0 (-grav*nt) )
 vetorV  = (Cartesiano ((cos (toRadianos incJ))*vj*tempo) (altJ + (sin (toRadianos incJ) * vj * tempo ) ) )
 vetorV2 = (Cartesiano (cos (toRadianos incJ)*vj*nt) (altJ + (sin (toRadianos incJ) * vj * nt ) ) )
 (Cartesiano (x) (y) )   = somaVetores vetorV vetorG
 (Cartesiano (x2) (y2) ) = somaVetores vetorV2 vetorG2
 nv  = vj * cos (toRadianos incJ)



-- |Primeiro ponto referente a um dado piso
ponto1 :: Peca -> Ponto
ponto1 (Recta _ y)  = Cartesiano 0 (fromIntegral y)
ponto1 (Rampa _ x y) = Cartesiano 0 (fromIntegral x)

-- |Ultimo ponto referente a um dado piso
ponto2 :: Peca -> Ponto
ponto2 (Recta _ y)  = Cartesiano 0.95 (fromIntegral y)
ponto2 (Rampa _ x y) = Cartesiano 0.95 (fromIntegral y)




