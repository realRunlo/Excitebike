-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g104 where

import LI11920
-- * Relatório
{-- 
->Intrudução<-
  Neste módulo era pretendido converter um mapa em uma sequência instruções.A dificuldade consistia em minimizar o número de instruções 
  dessa mesma sequência.

->Objetivo<-
  =1-Minimizar número de instruções
  =2-Função de conversão
  =3-Identificar padroes horizontais
  =4-Identificar e aplicar outros padrões (não realizado)

->Discussão e conclusão<-
   Incialmente começou-se pela função que converte cada peça num tipo de instrução.Sendo esta uma que função converte uma peça em uma instrução
  acaba-se por obter um número de instruções exatamente igual ao número de peças.De modo a obter um maior grau de compressão,procedeu-se á identificação de
  instruções consecutivas repetidas no decorrer de cada pista (padrões horizontais).Estas foram as estrategias possiveis de aplicar,porém,haviam outras que
  aliadas a estas tornariam a compressão mas eficaz  
  Conluindo,os resultados não foram os ideais,porém significativos.
--}

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [[[Recta Terra 0,Recta Relva 0,Recta Relva 0]],
            [[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 1],[Recta Terra 0,Rampa Boost 0 1,Rampa Boost 1 0,Recta Lama 0]],
            [[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 1]] ]
--[[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 1],[Recta Terra 0,Rampa Boost 0 1,Rampa Boost 1 0,Recta Lama 0]],[[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0],[Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 1]] ]


-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi [] = []
desconstroi mapa =  desconstroiAux mapa 0 
  
-- |Converte peças para intruções
converte::Peca->Int->Instrucao
converte (Recta piso x) np = (Anda [np] piso)
converte (Rampa piso x y) np | y-x>0 = (Sobe [np] piso (y-x))
converte (Rampa piso x y) np  = (Desce [np] piso (x-y))

-- |Executa recusividade da função converte
convRecusiva :: [Peca] -> Int -> Instrucoes
convRecusiva [] np = []
convRecusiva (h:[]) np = []
convRecusiva (h:t) np = (converte (head t) np):convRecusiva  t np

-- |Auxiliar da função descontroi 
desconstroiAux :: Mapa -> Int -> Instrucoes
desconstroiAux [] _ = []
desconstroiAux (h:t) np = (convRecusiva h np ) ++ desconstroiAux t (np+1)

-- |Identifica os padrões horizontais 
iPadroesH :: Instrucoes -> Instrucoes
iPadroesH [] = []
iPadroesH (h:[]) = [h]
iPadroesH (h:t) |h==head t = (Repete r [h]):iPadroesH (drop (r-1) t)
                |otherwise = h:iPadroesH t
 where
 r = 1 + repetido (h:t) 

-- |Conta o número de vezes que uma peça se repete 
repetido :: Instrucoes -> Int
repetido [] = 0
repetido (h:[])=0
repetido (h:t) |h==head t = 1 + repetido t
               |otherwise = 0


