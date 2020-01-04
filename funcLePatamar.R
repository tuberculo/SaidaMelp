#NomeArquivo <- "../Caso/ArquivosMelp/patamar.dat"
LePatamar <- function(NomeArquivo = "patamar.dat"){
  LinhasPatamar <- readLines(NomeArquivo, encoding = "latin1")
  
  LinhaInic <- grep("DURACAO MENSAL DOS PATAMARES DE CARGA", LinhasPatamar)[1] + 1
  LinhaFim <- grep("PROFUNDIDADE DOS PATAMARES", LinhasPatamar)[1] - 3
  if(!is.na(LinhaInic) & !is.na(LinhaFim)) {
    TextoPatamar <- LinhasPatamar[LinhaInic:LinhaFim]
  } else print(paste0("Não encontrado"))
  Patamar <- read_fwf(TextoPatamar, skip = 2, fwf_empty(TextoPatamar, skip = 2, col_names = c("Ano", 1:12))) # Lê os dados
  Patamar <- fill(Patamar, Ano) # Preenche anos em branco
  Patamar <- mutate(Patamar, Pat = 0:(nrow(Patamar)-1) %% 3 + 1)
  Patamar <- pivot_longer(Patamar, 2:13, names_to = "Mês", values_to = "DuraPat")
  Patamar$Mês <- parse_double(Patamar$Mês)
  #Calcula valor médio trimestral da duração do patamar
  Patamar <- mutate(Patamar, Trimestre = Mês/3)  
  Patamar$Trimestre <- ceiling(Patamar$Trimestre)
  Patamar <- group_by(Patamar, Ano, Pat, Trimestre) %>% summarise(DuraPatTri = mean(DuraPat)) %>% right_join(Patamar)
  Patamar
}