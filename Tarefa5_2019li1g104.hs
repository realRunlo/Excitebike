
-- |Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where
import Graphics.Gloss 
import Graphics.Gloss.Interface.Pure.Game
import Tarefa4_2019li1g104
import Tarefa2_2019li1g104 
import Graphics.Gloss.Juicy
import Tarefa0_2019li1g104
import LI11920
-- * Relatório
{-
->Intrudução<-
 Neste módulo era pretendido a criação de um interface gráfica do jogo,incorporando na mesma a tarefa 2 e a tarefa 4 como mecanismos do bom funcionamento 
 do jogo.

->Objetivos<-
1-Desenhar o Mapa
2-Desenhar os Jogadores
2-Aplicação das tarefas 2 e 4
3-Desenhar página de abertura

->Discussão e conclusão<-
 Concluindo,o jogo encontra-se funcional e possui tambem uma página inicial mas nenhum extra foi adicionado. 

-}

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
data EstadoGloss = Jogo (Estado,[Picture],[Picture],[Picture]) 
                   |Menu ([Picture],[Picture],[Picture],[Picture],(Float,Float))

type PosicaoJog = (Float,Float)


-- |Introduz as imagens no estado gloss do menu
estadoInicialGloss :: ([Picture],[Picture],[Picture],[Picture])->EstadoGloss
estadoInicialGloss (lm,li,lij,ll) = Menu (lm,li,lij,ll,(0,0))

-- |Estado inicial do jogo
estadoIJogo::Estado
estadoIJogo = (Estado [[Recta Terra 0,Recta Relva 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Relva 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Boost 0,Rampa Terra 0 1,Rampa Terra 1 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0],
                       [Recta Terra 0,Recta Relva 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Boost 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0]] 

                         [(Jogador 0 0 0 5 (Chao False)),(Jogador 1 0 0 5 (Chao False))])

--------------------------------------------------------Constrir mapa---------------------------------------------
-- |Desenha uma peça
desenhaPeca :: Peca ->Float->Float->[Picture]-> Picture
desenhaPeca (Recta piso _) x y (st:rp1T:rp1R:rp1L:rp1B:rp1C:rp2T:rp2R:rp2L:rp2B:rp2C:r1T:r1R:r1L:r1B:r1C:[]) = if piso==Terra then Scale scpeca scpeca (Translate x y r1T)
                                                                                                                 else if piso==Relva then Scale scpeca scpeca (Translate x y r1R)
                                                                                                                   else if piso==Lama then Scale scpeca scpeca (Translate x y r1L)
                                                                                                                    else if piso==Boost then Scale scpeca scpeca (Translate x y r1B)
                                                                                                                      else Scale scpeca scpeca (Translate x y r1C)
desenhaPeca (Rampa piso a b) x y (st:rp1T:rp1R:rp1L:rp1B:rp1C:rp2T:rp2R:rp2L:rp2B:rp2C:r1T:r1R:r1L:r1B:r1C:[])|a<b = if piso==Terra then Scale scpeca scpeca (Translate x (y+20) rp1T)
                                                                                                                     else if piso==Relva then Scale scpeca scpeca (Translate x (y+20) rp1R)
                                                                                                                       else if piso==Lama then Scale scpeca scpeca (Translate x (y+20) rp1L)
                                                                                                                         else if piso==Boost then Scale scpeca scpeca (Translate x (y+20) rp1B)
                                                                                                                           else Scale scpeca scpeca (Translate x (y+20) rp1C)
desenhaPeca (Rampa piso a b) x y (st:rp1T:rp1R:rp1L:rp1B:rp1C:rp2T:rp2R:rp2L:rp2B:rp2C:r1T:r1R:r1L:r1B:r1C:[])|a>b = if piso==Terra then Scale scpeca scpeca (Translate x (y+20) rp2T)
                                                                                                                     else if piso==Relva then Scale scpeca scpeca (Translate x (y+20) rp2R)
                                                                                                                       else if piso==Lama then Scale scpeca scpeca (Translate x (y+20) rp2L)
                                                                                                                         else if piso==Boost then Scale scpeca scpeca (Translate x (y+20) rp2B)
                                                                                                                           else Scale scpeca scpeca (Translate x (y+20) rp2C)
-- |Desenha uma pista peça por peça
desenhaPista :: Pista->Float->Float->[Picture]->[Picture]
desenhaPista [] _ _ _ = []
desenhaPista (h:[]) x y li = [(desenhaPeca h x y li)]
desenhaPista (h:t) x y li =  ((desenhaPeca h x y  li ):(desenhaPista t (x+40) y li)) 
-- |Desenha o a peça de começo da pista
desenhaPistaST :: Pista ->Float->Float->[Picture]->[Picture]
desenhaPistaST [] _ _ _ = []
desenhaPistaST (h:[]) x y li = [Scale 1.5 1.5 (Translate (x-40) y (head li))]
desenhaPistaST (h:t) x y li = (Scale 1.5 1.5 (Translate (x-40) y (head li))):desenhaPista t x y li
 
-- |Desenha o mapa pista por pista                              
desenhaMapa :: Mapa->Float->Float->[Picture]->[Picture]
desenhaMapa [[]] _ _ _  = []
desenhaMapa (h:[]) x y li = (desenhaPistaST h x y li)
desenhaMapa ((h:t):ts) x y li = ( (desenhaPistaST (h:t) x y li))++(desenhaMapa ts x (y-40) li) 

-- |Desenha o mapa começando numa dada posição
desenhaEstado :: Estado -> [Picture] -> [Picture]
desenhaEstado (Estado mapa lj) li =(desenhaMapa mapa (-400) (0) li)

-- |Desenha todo o estado incluindo o mapa e os jogadores
desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (Jogo ((Estado mapa lj),li,lij,ll)) = Pictures((desenhaEstado (Estado mapa lj) li)++(desenhaJogadores lj mapa lij ll))
desenhaEstadoGloss (Menu (lm,li,lij,ll,(x,y))) = Scale 1.43 1.4 (Translate x y (head lm))
-------------------------------------------------------Constantes------------------------------------------------
-- |Número de pixeis
nDP::Float
nDP = 40 * scpeca

-- |Scale usado nas peças
scpeca::Float
scpeca = 1.5

-- |Scale usado nos jogadores
sc::Float
sc = 0.2

-- |Frame rate
fr :: Int
fr = 50
------------------------------------------------------Desenhar Jogadores---------------------------------------

-- |Desenha o jogador tendo em conta a posição em que se encontra 
desenhaJogador:: Jogador->Mapa->Picture->Picture->Picture
desenhaJogador (Jogador 0 dj vj cj ej) mapa f mt  |estaNochao j = Translate (-676+d*nDP) (45+hp*nDP) (Rotate (-incP) (Scale sc sc f) ) 
                                                  |estaMorto j = Translate (-676+d*nDP) (45+hp*nDP) (Rotate (-incP) (Scale 0.1 0.1  mt) )
                                                  |otherwise = Translate (-676+d*nDP) (45+h*nDP) (Rotate (-inc) (Scale sc sc f) )
                                                 
                                                  
 where
 j = (Jogador 0 dj vj cj ej)
 p = (encontraPosicaoMatriz (0,(floor dj))  mapa )
 d = realToFrac dj
 hp = realToFrac (hPeca p dj)
 incP = realToFrac (inclinacaoPeca p)
 h = realToFrac (altJG ej)
 inc = realToFrac (incJG ej)
desenhaJogador (Jogador 1 dj vj cj ej) mapa f mt |estaNochao j = Translate (-676+d*nDP) (-35+hp*nDP) (Rotate (-incP) (Scale sc sc f) ) 
                                                 |estaMorto j = Translate (-676+d*nDP) (-35+hp*nDP) (Rotate (-incP) (Scale 0.1 0.1 mt) )
                                                 |otherwise = Translate (-676+d*nDP) (-35+h*nDP) (Rotate (-inc) (Scale sc sc f) )

  where
 j = (Jogador 1 dj vj cj ej)
 p = (encontraPosicaoMatriz (1,(floor dj))  mapa )
 d = realToFrac dj
 hp = realToFrac (hPeca p dj)
 incP = realToFrac (inclinacaoPeca p)
 h = realToFrac (altJG ej)
 inc = realToFrac (incJG ej)



-- |Desenha todos os jogadores
desenhaJogadores :: [Jogador]->Mapa->[Picture]->[Picture]->[Picture]
desenhaJogadores [] _ _ _ = []
desenhaJogadores  _ _ [] _ = []
desenhaJogadores (h:t) mapa (x:xs) ll = (desenhaJogador  h mapa x (head ll)):(desenhaJogadores t mapa xs ll)

------------------------------------------------------------------Eventos---------------------------------------
-- |Dado um evento do tecldo reage em conformidade
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss (EventKey (SpecialKey KeySpace) Down a b) (Menu (lm,li,lij,ll,(0,0))) = Jogo (estadoIJogo,li,lij,ll)
reageEventoGloss ev (Jogo (estado,li,lij,ll)) = (Jogo (reageEvento ev estado,li,lij,ll))
reageEventoGloss _ s = s

-- |Tendo em conta a tecla pressionada aplica no estado essa jogada
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) estado = jogada 0 (Movimenta C) estado
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) estado = jogada 0 (Movimenta B)  estado
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) estado = jogada 0 (Movimenta E) estado
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) estado = jogada 0 (Movimenta D) estado
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) estado = jogada 0 (Acelera) estado 
reageEvento (EventKey (Char 'p') Down _ _)            estado = jogada 0 (Dispara) estado
reageEvento (EventKey (Char 'w') Down _ _) estado  = jogada 1 (Movimenta C) estado
reageEvento (EventKey (Char 's') Down _ _) estado  = jogada 1 (Movimenta B) estado
reageEvento (EventKey (Char 'a') Down _ _) estado  = jogada 1 (Movimenta E) estado
reageEvento (EventKey (Char 'd') Down _ _) estado  = jogada 1 (Movimenta D) estado
reageEvento (EventKey (Char 'q') Down _ _) estado  = jogada 1 (Acelera) estado
reageEvento (EventKey (Char 'e') Down _ _) estado  = jogada 1 (Dispara) estado
reageEvento _ s = s -- ignora qualquer outro evento                           
-------------------------------------------------Tempo-------------------------------------------
-- |Altera o estado gloss conforme a passagem do tempo
reageTempoGloss :: Float-> EstadoGloss -> EstadoGloss
reageTempoGloss n (Jogo (Estado mapa lj,li,lij,ll)) = (Jogo (Estado mapa (reagePasso n mapa lj),li,lij,ll))                                                                         
reageTempoGloss n (Menu s) = Menu s

-- |Aplica a função passo recursivamente a uma lista de jogadores
reagePasso :: Float ->Mapa->[Jogador] -> [Jogador]
reagePasso _ _ [] = []
reagePasso n mapa (h:t) = (passo (realToFrac n) mapa h):reagePasso n mapa t

----------------------------------------------Auxiliares-----------------------------------
-- |Devolve a altura de uma peça numa dada distância
hPeca::Peca->Double->Double
hPeca (Recta _ x ) _ = fromIntegral x
hPeca (Rampa _ x y) d = fromIntegral x + (fromIntegral (y-x)) * (d-fromIntegral (floor d))

-- |Devolde a altura de um jogador
altJG :: EstadoJogador->Double
altJG (Ar y _ _) = y
altJG _ = 0

-- |Devolde a inclinação de um jogador
incJG :: EstadoJogador->Double
incJG (Ar _ i _) = i
incJG _ = 0


-- |Tamanho da janela onde roda o jogo
janela::Display
janela = FullScreen


main :: IO ()
main = do                  
     Just st  <-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerStart.png"
     Just rp1T<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRampaLama.png"
     Just rp1R<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRampaTerrab.png"
     Just rp1L<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRampaTerrab.png"
     Just rp1B<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRampaTerrab.png"
     Just rp1C<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/RampaCola.jpg"
     Just rp2T<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRampaLamab.png"
     Just rp2R<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRampaTerrab.png"
     Just rp2L<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRampaTerrab.png"
     Just rp2B<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRampaTerrab.png"
     Just rp2C<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/RampaColab.png"
     Just r1T <-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRectaTerra.png"
     Just r1R <-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/Relva2.png"
     Just r1L <-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/Lama.png"
     Just r1B <-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/LayerRectaBoost.png"
     Just r1C <-loadJuicy "/home/runlo/2019li1g104/src/Imagens/texturas/cola.png"
     Just p1  <-loadJuicy  "/home/runlo/2019li1g104/src/Imagens/playersIm/mota1.png"
     Just p2  <-loadJuicy  "/home/runlo/2019li1g104/src/Imagens/playersIm/mota2.png"
     Just fundoSt<-loadJuicy "/home/runlo/2019li1g104/src/Imagens/fundos/fundoVn.png"
     Just morto  <-loadJuicy "/home/runlo/2019li1g104/src/Imagens/playersIm/cruz.png"
     play janela
          (greyN 0.5)          -- côr do fundo da janela
          fr                   -- frame rate
          (estadoInicialGloss ([fundoSt],[st,rp1T,rp1R,rp1L,rp1B,rp1C,rp2T,rp2R,rp2L,rp2B,rp2C,r1T,r1R,r1L,r1B,r1C],[p1,p2],[morto]))   -- estado inicial
          desenhaEstadoGloss   -- desenha o estado do jogo
          reageEventoGloss     -- reage a um evento
          reageTempoGloss      -- reage ao passar do tempo


