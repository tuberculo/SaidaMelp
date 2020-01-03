NomeArquivo <- "Melp.out"
LinhasRelat <- readLines(NomeArquivo, encoding = "latin1")

LinhaInic <- grep("\\(RELAT06\\)", LinhasRelat)[1] + 2
LinhaFim <- grep("\\(RELAT07\\)", LinhasRelat)[1] - 2

if(!is.na(LinhaInic) & !is.na(LinhaFim)) {
  TextoRelat06 <- LinhasRelat[LinhaInic:LinhaFim]
} else print(paste0("Não encontrado"))

SepAno <- c(grep("ANO:  .... - ESTACAO:", TextoRelat06), NROW(TextoRelat06)) # ìndice com os inícios de cada ano

#Dados obtidos usando dput(fwf_empty(TextoAno, skip = 5)) e alterados manualmente.
SepCols <- list(begin = c(0L, 4L, 6L, 8L, 11L, 21L, 32L, 43L, 54L, 65L, 76L, 87L), 
                end = c(3L, 5L, 7L, 10L, 20L, 31L, 42L, 53L, 64L, 75L, 86L, NA), 
                skip = 0L, col_names = c("Subsistema", "X2", "Patamar", "Estacao", 
           "GeraHidr", "GeraTer", "Import", "Export", "Deficit", "Mercado", "Perdas", "Custo Operacional"))

BalancoTotal <- NULL
for (i in 1:(NROW(SepAno) - 1)){
  TextoAno <- TextoRelat06[SepAno[i]:(SepAno[i+1]-6)]
  Ano <- parse_double(substr(TextoAno[1], 7, 10)) # Lê o ano.
  BalancoAno <- read_fwf(TextoAno, SepCols, skip = 5, col_types = "n_ncdddddddd") # Separa as colunas
  BalancoAno <- drop_na(fill(BalancoAno, c("Subsistema", "Patamar"))) # Preenche as linhas em branco com dados de Subsistema e patamar
  BalancoAno <- separate(BalancoAno, Estacao, c("Energia", "Estacao"), 1, convert = TRUE) # Separa dado de tipo de energia do dado de estação sazonal
  BalancoAno$Energia <- BalancoAno$Energia <- gsub("M", "Média", gsub("C", "Crítica", BalancoAno$Energia)) # Transforma C e M em "Crítica" e "Média
  BalancoAno <- add_column(BalancoAno, Ano, .before = TRUE) # Coluna com ano
  BalancoTotal <- bind_rows(BalancoTotal, BalancoAno) # Junta com os dados dos outros anos.
}

# Converte para unidade de energia de acordo com o patamar:
BalancoTotal <- left_join(BalancoTotal, PatamarTrim, by = c("Ano", "Patamar" = "Pat", "Estacao" = "Trimestre")) %>% mutate(GeraHidrEnerg = GeraHidr * PatTri, GeraTerEnerg = GeraTer * PatTri)

#ANO:  2019 - ESTACAO: