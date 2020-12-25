-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g104 where
import Tarefa0_2019li1g104 
import LI11920

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0,Acelera,Estado m1 js1),(0,Desacelera,Estado m1 js1),(0,Movimenta C,Estado m1 js1),(0,Movimenta B,Estado m1 js1),(0,Movimenta E,Estado m1 js1),(0,Movimenta D,Estado m1 js1),(0,Dispara,Estado m1 js1),
            (1,Acelera,Estado m1 js1),(1,Desacelera,Estado m1 js1),(1,Movimenta C,Estado m1 js1),(1,Movimenta B,Estado m1 js1),(1,Movimenta E,Estado m1 js1),(1,Movimenta D,Estado m1 js1),(1,Dispara,Estado m1 js1),
            (2,Acelera,Estado m1 js1),(2,Desacelera,Estado m1 js1),(2,Movimenta C,Estado m1 js1),(2,Movimenta B,Estado m1 js1),(2,Movimenta E,Estado m1 js1),(2,Movimenta D,Estado m1 js1),(2,Dispara,Estado m1 js1),
            (3,Acelera,Estado m1 js1),(3,Desacelera,Estado m1 js1),(3,Movimenta C,Estado m1 js1),(3,Movimenta B,Estado m1 js1),(3,Movimenta E,Estado m1 js1),(3,Movimenta D,Estado m1 js1),(3,Dispara,Estado m1 js1),
            (4,Acelera,Estado m1 js1),(4,Desacelera,Estado m1 js1),(4,Movimenta C,Estado m1 js1),(4,Movimenta B,Estado m1 js1),(4,Movimenta E,Estado m1 js1),(4,Movimenta D,Estado m1 js1),(4,Dispara,Estado m1 js1),
            (5,Acelera,Estado m1 js1),(5,Desacelera,Estado m1 js1),(5,Movimenta C,Estado m1 js1),(5,Movimenta B,Estado m1 js1),(5,Movimenta E,Estado m1 js1),(5,Movimenta D,Estado m1 js1),(5,Dispara,Estado m1 js1)]
      


-- | Mapa teste
m1=[[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 1],[Recta Terra 0,Rampa Boost 0 1,Rampa Boost 1 0,Recta Lama 0],[Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0 ,Rampa Relva 0 1,Recta Terra 1],[Recta Terra 0,Recta Lama 0 , Recta Relva 0 ,Rampa Terra 0 1]]
-- | Lista teste de jogadores
js1=[jogador0,jogador1,jogador2,jogador3,jogador4,jogador5]
 where
 jogador0= Jogador 0 2.3 2 5 (Chao True)
 jogador1= Jogador 1 3 2 5 (Chao True)
 jogador2= Jogador 2 3.3 2 5 (Chao True)
 jogador3= Jogador 3 2.3 2 4 (Ar 4 15 0)
 jogador4= Jogador 4 2.9 2 0 (Chao True)
 jogador5= Jogador 5 2.5 2 3  (Chao True)
 

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada i jogada (Estado m js) = if jogada==Movimenta C  then funcaoMovimenta i C (Estado m js)
                                  else if jogada==Movimenta B  then funcaoMovimenta i B (Estado m js)
                                    else if jogada==Movimenta E  then funcaoMovimenta i E (Estado m js)
                                     else if jogada==Movimenta D  then funcaoMovimenta i D (Estado m js)
                                  else if jogada==Acelera then funcaoAcelera i (Estado m js)
                              else if jogada==Desacelera then funcaoDesacelera i (Estado m js)
                            else funcaoDispara i (Estado m js)

-- | Função para desecelerar
funcaoAcelera :: Int->Estado->Estado
funcaoAcelera  i (Estado m js) |estaMorto j || (estaNochao j) == False = (Estado m js)
                               |otherwise = (Estado m (atualiza i nj js))
 where
 j=mostraJogador i (Estado m js)
 nj = aumentaAcel j 
-- |concretiza o efeito no estado da aceleração
aumentaAcel::Jogador->Jogador
aumentaAcel (Jogador pj dj vj cj ej) = (Jogador pj dj vj cj (Chao True)) 

-- |Função para acelerar
funcaoDesacelera :: Int -> Estado->Estado
funcaoDesacelera  i (Estado m js) |estaMorto j || not (estaNochao j) = (Estado m js)
                                  |otherwise = (Estado m (atualiza i nj js))


 where
 j=mostraJogador i (Estado m js)
 nj = diminuiAcel j 
-- |concretiza o efeito no estado da desaceleração
diminuiAcel ::Jogador->Jogador
diminuiAcel (Jogador pj dj vj cj ej) = (Jogador pj dj vj cj (Chao False)) 


-- *Função para o movimento

-- |Função de decisão do movimento a tomar
funcaoMovimenta::Int ->Direcao->Estado->Estado
funcaoMovimenta i dirc (Estado m js) = if dirc==C then cima i j (Estado m js) 
                                          else if dirc==B then baixo i j (Estado m js)
                                            else if dirc==E then esquerda i j (Estado m js)
                                           else direita i j (Estado m js)
 where
 j=mostraJogador i (Estado m js)

-- |Move para a pista à esquerda
cima :: Int->Jogador -> Estado->Estado   
cima i (Jogador pj dj vj cj ej) (Estado m js) = if estaMorto j || not (estaNochao j) || pj==0 then Estado m js
                                                 else if difAlt <= 0.2 then Estado m (atualiza i nj1 js)
                                                     else if difAlt >0.2 && altPecaJogador<=(getAltFPec pecaIr) then (Estado m (atualiza i nj2 js)  )
                                                    else Estado m (atualiza i nj3 js)
 where
 j=mostraJogador i (Estado m js)
 alturaJogador = getAltJ pecaJ dj ang
 altPecaJogador = getAltFPec pecaJ
 ang = atan (getAltFPec pecaJ)
 pecaJ = encontraPosicaoMatriz (pj,floor dj) m
 pecaIr = encontraPosicaoMatriz (pj-1,floor dj) m
 difAlt = abs (alturaJogador - getAltFPec pecaIr)
 nj1=(Jogador (pj-1) dj vj cj ej)
 nj2=(Jogador pj dj 0 cj (Morto 1.0))
 nj3=(Jogador (pj-1) dj vj cj (Ar alturaJogador  (toGraus ang)  0)  )
 
-- |Move para a pista á direita
baixo :: Int->Jogador -> Estado->Estado   
baixo i (Jogador pj dj vj cj ej) (Estado m js) = if estaMorto j || not (estaNochao j) || pj==((length js)-1) then Estado m js
                                                   else if difAlt <=0.2 then Estado m (atualiza i nj1 js)
                                                     else if difAlt >0.2 && altPecaJogador<=(getAltFPec pecaIr) then (Estado m (atualiza i nj2 js)  )
                                                    else Estado m (atualiza i nj3 js)
 where
 j = mostraJogador i (Estado m js)                                         
 alturaJogador = getAltJ pecaJ dj ang
 altPecaJogador = getAltFPec pecaJ 
 ang = atan (getAltFPec pecaJ)
 pecaJ = encontraPosicaoMatriz (pj,floor dj) m
 pecaIr = encontraPosicaoMatriz (pj+1,floor dj) m
 difAlt = abs (alturaJogador - getAltFPec pecaIr)
 nj1=(Jogador (pj+1) dj vj cj ej)
 nj2=(Jogador pj dj 0 cj (Morto 1.0))
 nj3=(Jogador (pj+1) dj vj cj (Ar alturaJogador (toGraus ang) 0)  )

-- |Diminui inclinação
esquerda::Int->Jogador ->Estado->Estado 
esquerda i (Jogador pj dj vj cj ej) (Estado m js) = if estaMorto j || estaNochao j||inclinacaoJ<= -90 ||inclinacaoJ>= 90 ||(inclinacaoJ +15)<= -90 then Estado m js
                                                       else (Estado m (atualiza i nj1 js))
 where
 j=mostraJogador i (Estado m js)
 inclinacaoJ=ecliEjGet (Jogador pj dj vj cj ej)
 alturaJ= altEjGet (Jogador pj dj vj cj ej)
 nj1=(Jogador pj dj vj cj (Ar alturaJ (inclinacaoJ+15) 0))
-- |Aumenta inclinação
direita::Int->Jogador->Estado->Estado
direita i (Jogador pj dj vj cj ej) (Estado m js) = if estaMorto j || estaNochao j||inclinacaoJ<= -90 ||inclinacaoJ>=90||(inclinacaoJ - 15)<= -90 then Estado m js
                                                       else (Estado m (atualiza i nj1 js))

 where
 j=mostraJogador i (Estado m js)
 inclinacaoJ=ecliEjGet (Jogador pj dj vj cj ej)
 alturaJ= altEjGet (Jogador pj dj vj cj ej)
 nj1=(Jogador pj dj vj cj (Ar alturaJ (inclinacaoJ-15) 0))

-- |Devolve a altura de um estado ar                                                      
altEjGet::Jogador->Double
altEjGet (Jogador pj dj vj cj (Ar a b c)) = a
-- |Devolve a inclinação de um estado ar 
ecliEjGet::Jogador->Double
ecliEjGet (Jogador pj dj vj cj (Ar a b c)) = b

-- *Função para disparo de cola
-- |Concretiza o efeito do disparo no mapa e no estado do jogador 
funcaoDispara :: Int->Estado->Estado
funcaoDispara i (Estado m js) = if estaMorto j || not (estaNochao j)|| not (temBalas j) then (Estado m js)
                                   else mudaPisoAnt i j (Estado m js)

 where
 j=mostraJogador i (Estado m js)
 
-- |Altera o piso anterior e deconta uma munição
mudaPisoAnt ::Int->Jogador -> Estado ->Estado
mudaPisoAnt i (Jogador pj dj vj cj ej) (Estado m js) |(dj>=0 && dj<1) = (Estado m js)
                                                     |otherwise = (Estado nm nj)
                                                   
 where
 nm=atualizaPosicaoMatriz(pj,(floor dj)-1 ) pec m
 pec=mudaPecAnt (Jogador pj dj vj cj ej) (Estado m js)
 nj=atualiza i (Jogador pj dj vj (cj-1) ej) js
                              
-- |Encontra uma peça no mapa e altera essa peca usando a função mudapeca
mudaPecAnt :: Jogador -> Estado -> Peca
mudaPecAnt (Jogador pj dj vj cj ej) (Estado m js) = p
 where
 p=mudaPeca (encontraPosicaoMatriz (pj,(floor dj)-1)  m )

-- |Função que altera o piso de uma peça para cola
mudaPeca :: Peca -> Peca
mudaPeca (Rampa piso x y) = Rampa Cola x y  
mudaPeca (Recta piso x) = Recta Cola x


-- *Funções para manipulação de listas e matrizes
-- |Defenição do tipo posição matriz
type PosicaoMatriz = (Int,Int)
-- |Defenição do tipo matriz
type Matriz a = [[a]]
-- |Atualiza o jogador na lista de jogadores                                                   
atualiza::Int->Jogador->[Jogador]->[Jogador]
atualiza i j (h:t) = if i==0 then (j:t) else h:atualiza (i-1) j t


-- *Funções auxiliares 

-- |Testa se o jogador vivo ou morto
estaMorto::Jogador->Bool
estaMorto (Jogador pj dj vj cj (Morto x))  = True     
estaMorto (Jogador pj dj vj cj ej)  = False 

-- |Testa se o jogador no chao ou nao
estaNochao:: Jogador->Bool
estaNochao (Jogador pj dj vj cj (Chao x)) = True
estaNochao (Jogador pj dj vj cj ej) = False                                               

-- |Recebe identificador devolve o jogador correspondente
mostraJogador::Int->Estado->Jogador
mostraJogador i (Estado m js) =encontraIndiceLista i js  
                     --recebe o identificador vai à lista e devolve o jogador
-- |Testa se o jogador tem balas ou não      
temBalas :: Jogador->Bool
temBalas (Jogador pj dj vj cj ej) = if (cj>0) then True
                                       else False                     
-- * Função de conversão
-- |Converte de radianos para graus                               
toGraus :: Double -> Double
toGraus x = (x * 180)/pi

-- * Funções de busca de alturas
-- |Devolve a altura exata no jogador na peça
getAltJ ::Peca ->Double-> Double->Double
getAltJ (Recta pista x) dj ang = fromIntegral x
getAltJ (Rampa pista x y) dj ang = tan ang * (dj-(fromIntegral (floor dj)))
-- |Devolve a altura final de uma peça
getAltFPec::Peca -> Double
getAltFPec (Recta pista x) = fromIntegral x
getAltFPec(Rampa pista x y) = fromIntegral y



