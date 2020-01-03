library(tidyverse)
source("funcLePatamar.R")

Patamar <- LePatamar("../Caso060/ArquivosMelp/patamar.dat")
Patamar$Mês <- parse_double(Patamar$Mês)
Patamar <- mutate(Patamar, Trimestre = Mês/3) 
Patamar$Trimestre <- ceiling(Patamar$Trimestre)
PatamarTrim <- group_by(Patamar, Ano, Pat, Trimestre) %>% summarise(PatTri = mean(DuraPat))

CaminhoArquivoTermOper <- "../Caso060/MelpResul/TermOper.out"
nomeCols <- unlist(read_fwf(CaminhoArquivoTermOper, skip = 8, n_max = 1, fwf_empty(CaminhoArquivoTermOper, skip = 8, n = 1000L)))
TermOperLido <- read_fwf(CaminhoArquivoTermOper, skip = 10, fwf_empty(CaminhoArquivoTermOper, skip = 8, n = 1000L, col_names = nomeCols))
ColsRepetirVal <- colnames(TermOperLido[1:11] %>% select(-NPAT, -PERZ)) # Nome das colunas para repetir valores
TermOperLido <- drop_na(TermOperLido, NPAT, PERZ) # Retira linhas que não têm dados de geração 
TermOperLido <- fill(TermOperLido, ColsRepetirVal)

# Sepora as tabelas de cenário médio e cenário crítico
Cmed <- TermOperLido[1:(nrow(TermOperLido)/2),] 
Ccrit <- TermOperLido[(nrow(TermOperLido)/2 + 1):nrow(TermOperLido),]

ColGer <- grep("GER.(?=[[:digit:]])", colnames(Cmed), perl = TRUE) # Guarda quais colunas têm dados de geração
Cmed <- select_all(Cmed, ~gsub("GER.(?=[[:digit:]])", "", ., perl = TRUE)) # Renomeia as colunas com dados de geração
Ccrit <- select_all(Ccrit, ~gsub("GER.(?=[[:digit:]])", "", ., perl = TRUE)) # Renomeia as colunas com dados de geração

Cmed <- pivot_longer(Cmed, ColGer, names_to = "Ano", values_to = "Gera") 
Ccrit <- pivot_longer(Ccrit, ColGer, names_to = "Ano", values_to = "Gera")

Cmed <- select(Cmed, -NPAT, -PERZ, -Ano, -Gera, Ano, PERZ, Pat = NPAT, GeraPot = Gera) # Muda ordem
Ccrit <- select(Ccrit, -NPAT, -PERZ, -Ano, -Gera, Ano, PERZ, Pat = NPAT, GeraPot = Gera) # Muda ordem
#Ccrit <- rename(Ccrit, ENER.CRIT = ENER.MED) # Renomeia coluna de energia crítica

GeraTerm <- bind_rows("Média" = Cmed, "Crítica" = Ccrit, .id = "Energia")
GeraTerm <- rename(GeraTerm, Med_Crit = ENER.MED) # Renomeia coluna de energia crítica/média
GeraTerm$Ano <- parse_double(GeraTerm$Ano)
GeraTerm <- left_join(GeraTerm, PatamarTrim, by = c("Ano" = "Ano", "Pat" = "Pat", "PERZ" = "Trimestre"))
GeraTerm <- mutate(GeraTerm, GeraEner = GeraPot * PatTri) # Geração em energia
