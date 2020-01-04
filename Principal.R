library(tidyverse)
source("funcLePatamar.R") # Carrega a função de leitura de duração de patamar.
source("LeTermOper.R") # Carrega a função de leitura de operação de termelétricas.
source("FuncLeRelat06.R") # Carrega a função de leitura de relat06 (geração UHE e UTE).

Patamar <- LePatamar("../Caso060/ArquivosMelp/patamar.dat") #Lê arquivo de patamar.
Term <- LeTermOper("../Caso060/MelpResul/TermOper.out") # Lê operação termelétrica
Balanco <- LeRelat06("../Caso060/MelpResul/Melp.out") # Lê relat06
