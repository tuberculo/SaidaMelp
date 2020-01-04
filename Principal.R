library(tidyverse)
source("funcLePatamar.R") # Carrega a função de leitura de duração de patamar.
source("LeTermOper.R") # Carrega a função de leitura de operação de termelétricas.
source("FuncLeRelat06.R") # Carrega a função de leitura de relat06 (geração UHE e UTE).

PatamarTudo <- NULL
TermTudo <- NULL
BalancoTudo <- NULL
for (i in 60:60){
  Patamar <- LePatamar(sprintf("../Caso%03.0f/ArquivosMelp/patamar.dat", i)) #Lê arquivo de patamar.
  Term <- LeTermOper(sprintf("../Caso%03.0f/MelpResul/TermOper.out", i)) # Lê operação termelétrica
  Balanco <- LeRelat06(sprintf("../Caso%03.0f/MelpResul/Melp.out", i)) # Lê relat06
  
  #Adiciona número do caso e anexa à tabela de total.
  Patamar <- add_column(Patamar, Caso = i, .before = TRUE)
  PatamarTudo <- bind_rows(PatamarTudo, Patamar)
  Term <- add_column(Term, Caso = i, .before = TRUE)
  TermTudo <- bind_rows(TermTudo, Term)
  Balanco <- add_column(Balanco, Caso = i, .before = TRUE)
  BalancoTudo <- bind_rows(BalancoTudo, Balanco)
}

# Soma das gerações de UHE e UTE por ano:
GerAnualporTipo <- group_by(Balanco, Ano, Patamar, Subsistema) %>% filter(Energia == "Média") %>% summarise(Hid = mean(GeraHidrEnerg), Ter = mean(GeraTerEnerg)) %>% ungroup() %>% group_by(Ano) %>% summarise(HidrTot = sum(Hid), TermTot = sum(Ter))
