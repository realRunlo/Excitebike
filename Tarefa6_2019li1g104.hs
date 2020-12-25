-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g104 where
import Tarefa2_2019li1g104
import LI11920
import Tarefa0_2019li1g104

-- * Relatório
{-
->Intrudução<-
 Neste módulo era pretendido a criação de um bot,através de estratégias previamente implantadas,sendo este de uma forma "inteligente" 
capaz competir no jogo proposto.

->Objetivos<-
1-Caso estado seja False o bot acelera mudando o para True
2-Identifição do atrito das peças que se encontram à frente e nas laterais dessa peça
2-Verificar o estado,caso esteja no ar ajusta a inclinação
3-Disparar por para Boosts ou para jogadores que se encontram na mesma peça

->Discussão e conclusão<-
 Com a realização desta tarefa deu para perceber que quanto mais visão do mapa e mais "inteligência" na mudança da inclinação melhores
seriam os resultados.No entanto,essa apllicação não foi muito bem conseguida.

-}


-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot id (Estado mapa lj) =  if (ej==(Chao False)) then anda  
                             else if (estaNochao j) then ajustaDirecao j mapa 
                              else if (not (estaNochao j)) then mudaInc j
                             else tiro j mapa lj 
                                      
 where
 j = encontraIndiceLista id lj
 ej = estadoJogador j

anda :: Maybe Jogada
anda = Just Acelera

-- |Identifica o atrito das peças e move para a de menor atrito
ajustaDirecao :: Jogador->Mapa->Maybe Jogada
ajustaDirecao (Jogador pj dj vj cj ej) mapa |(pj==0) = let peca1'= encontraPosicaoMatriz (pj,floor dj+1) mapa
                                                           peca2' = encontraPosicaoMatriz (pj+1,floor dj+1) mapa
                                                       in  muda2 (Jogador pj dj vj cj ej)  peca1' peca2'
                                            |pj==((length mapa) -1) = let peca1'' = encontraPosicaoMatriz (pj,floor dj+1) mapa
                                                                          peca2'' = encontraPosicaoMatriz (pj-1,floor dj+1) mapa
                                                                      in  muda2 (Jogador pj dj vj cj ej)  peca1'' peca2''
                                            |otherwise = let peca1=encontraPosicaoMatriz (pj-1,floor dj+1) mapa
                                                             peca2=encontraPosicaoMatriz (pj,floor dj+1) mapa
                                                             peca3=encontraPosicaoMatriz (pj+1,floor dj+1) mapa
                                                         in  muda peca1 peca2 peca3

-- |Compara os atritos das 3 peças,a da frente e das peças laterais á frente
muda :: Peca->Peca->Peca-> Maybe Jogada
muda p1 p2 p3 = if (menor==p1') then Just (Movimenta E)
                    else if (menor==p2') then Nothing
                       else Just (Movimenta D)  
    where
    p1'=infoAtrito p1
    p2'=infoAtrito p2
    p3'=infoAtrito p3
    menor = min (min p1' p2') p3'
-- |Compara os atritos quando o jogador se encontra na primeira ou ultima pista
muda2 ::Jogador->Peca -> Peca -> Maybe Jogada
muda2 (Jogador pj dj vj cj ej) p1 p2 |(pj==0) = if (menor==p1') then Nothing else Just (Movimenta D)
                                     |otherwise = if (menor==p1') then Nothing else Just (Movimenta E)
 where
 p1'=infoAtrito p1
 p2'=infoAtrito p2
 menor = (min p1' p2') 
  

-- |Retorna atrito de uma peça
infoAtrito :: Peca -> Double
infoAtrito  peca    |piso==Terra = 0.25
                    |piso==Relva = 0.75
                    |piso==Lama  = 1.5
                    |piso==Boost = -0.5
                    |otherwise   = 3.00
  where
  piso=getPiso peca

-- |Retorna piso de uma peça
getPiso :: Peca->Piso
getPiso (Recta piso _) = piso
getPiso (Rampa piso _ _) = piso


-- |Altera inclinacao
mudaInc :: Jogador -> Maybe Jogada
mudaInc j   |incJ==10 = Nothing
            |incJ>10 = Just (Movimenta B)
            |otherwise = Just (Movimenta C)
 where
 incJ=ecliEjGet j 

-- |Bot dispara para cima de uma peça Boost ou quando tem um jogador atrás
tiro :: Jogador -> Mapa -> [Jogador] -> Maybe Jogada
tiro (Jogador pj dj vj cj ej) mapa lj = if pisoAnt==Boost then Just (Dispara)
                                           else if tiroJogador (Jogador pj dj vj cj ej) lj then Just (Dispara)
                                          else Nothing
    where
    pisoAnt =getPiso (encontraPosicaoMatriz (pj,floor dj-1) mapa)

-- |Testar se existem jogadores na peça anterior á peça atual
tiroJogador :: Jogador-> [Jogador]->Bool
tiroJogador j  lj |(atras dj (floor (distanciaJogador p1))) && (mesmaPista pj (pistaJogador p1)) = True
                  |(atras dj (floor (distanciaJogador p2))) && (mesmaPista pj (pistaJogador p2)) = True
                  |(atras dj (floor (distanciaJogador p3))) && (mesmaPista pj (pistaJogador p3)) = True
                  |(atras dj (floor (distanciaJogador p4))) && (mesmaPista pj (pistaJogador p4)) = True
                  |otherwise = False
  where
  pj=pistaJogador j
  dj=floor (distanciaJogador j)
  p1=(encontraIndiceLista 0 lj)
  p2=(encontraIndiceLista 1 lj)
  p3=(encontraIndiceLista 2 lj)
  p4=(encontraIndiceLista 3 lj)

-- |Aux da função tiroJogador
atras::Int->Int->Bool
atras a b = if b==(a-1) then True else False
-- |Aux da função tiroJogador
mesmaPista:: Int -> Int -> Bool
mesmaPista a b = if a==b then True else False