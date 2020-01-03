library(tidyverse)
Melpout <- readLines("Melp.out", encoding = "latin1")
LinhaInic <- grep("RELAT03", Melpout)[2] - 1
LinhaFim <- grep("RELAT04", Melpout)[2] - 2
Relat03 <- Melpout[LinhaInic:LinhaFim]

LeRelat3(Relat03, 7)


# Função LêRelat3 ---------------------------------------------------------
# Lê os dados do RELAT03 e exporta os dados para arquivos CSV.
LeRelat3 <- function(NomeArquivo = Relat03, Caso = 1){
  colunas <- c(5,6,7,15,12,12,12,11,6) # Intervalos entre as colunas
  NomesCol <- c("Ano","Subsistema","Tipo","Nome","% investido","Pot.Ins.","E. Crit.","E. Med.","OBR ou OPT")
  
  relat3 <- read_fwf(NomeArquivo, fwf_widths(colunas,NomesCol), skip = 11, col_types = "iiccdcddc") # Lê pulando as primeiras 11 linhas
  relat3 <- filter(relat3, !is.na(Ano)) # Remova as linhas com NA. São as linhas sem informação.
  relat3 <- filter(relat3, Tipo == "Intr.") %>% separate(Pot.Ins., into = c("De", "Para")) %>% 
    right_join(relat3) # Separa os "De-Para" das linhas de transmissão.
  
  relat3$Ano <- parse_date(as.character(relat3$Ano), format = "%Y")
  #relat3$Ano <- parse_datetime(as.character(relat3$Ano), format = "%Y")
  relat3$`OBR ou OPT` <-  parse_factor(relat3$`OBR ou OPT`, levels = unique(relat3$`OBR ou OPT`))
  #relat3$Tipo <-  parse_factor(relat3$Tipo, levels = unique(relat3$Tipo))
  relat3$Pot.Ins. <- parse_double(relat3$Pot.Ins.)
  
  
  # dividindo "térmicas" por fonte
  # ^ faz com que apenas o início da string seja considerada
  relat3$Tipo[grepl("^EOL", relat3$Nome, ignore.case = TRUE)] <- "Eolica"
  relat3$Tipo[grepl("^BAG", relat3$Nome, ignore.case = TRUE)] <- "Biomassa"
  relat3$Tipo[grepl("^SOL", relat3$Nome, ignore.case = TRUE)] <- "Fotovoltaica"
  relat3$Tipo <-  parse_factor(relat3$Tipo, levels = unique(relat3$Tipo))
  
  DadosRodada <- read_fwf(paste(read_lines(NomeArquivo, skip = 6, n_max = 1), "\n"), fwf_cols(Investmento = c(9,23), Operação = c(36,50), Total = c(63,77), Tempo = c(96,106), Gap = c(117,126)))
  DadosRodada <- mutate(DadosRodada, TempoHora = Tempo/3600)

  relat3 <- add_column(relat3, Caso = Caso, .before = TRUE)
  DadosRodada <- add_column(DadosRodada, Caso = Caso, .before = TRUE)
  ImprimeColunas <- !file.exists("ResultadosRelat3.csv")
  write_csv2(relat3, "ResultadosRelat3.csv", append = TRUE, col_names = ImprimeColunas)
  write_csv2(DadosRodada, "DadosdaRodada.csv", append = TRUE, col_names = ImprimeColunas)
}


