library(tidyverse)

PatamarTudo <- readRDS("PatamarTudo.rds")
TermTudo <- readRDS("TermTudo.rds")
BalancoTudo <- readRDS("BalancoTudo.rds")


TermTudo <- mutate(TermTudo, Tipo = case_when(grepl("EOL", NOME) ~ "Eol", 
                                              grepl("GAS", NOME) ~ "Gas",
                                              grepl("CARV", NOME) ~ "Carvao",
                                              grepl("SOL", NOME) ~ "UFV",
                                              grepl("CSP", NOME) ~ "CSP",
                                              grepl("BAG", NOME) ~ "Bagaco",
                                              grepl("NUC", NOME) ~ "Nuclear",
                                              grepl("ANGRA", NOME) ~ "Nuclear",
                                              TRUE ~ "OutrosExistentes"))

GerAnualporTipoTerm <- group_by(TermTudo, Caso, Energia, Ano, Tipo) %>% summarise(Gera = sum(GeraEner) / 12)

# Plot --------------------------------------------------------------------
# Define os casos:
# IS22 é 54
# IS21 é 56
# IS25 é 57
Caso_min <- 51
#escolhidos <- c("Referência","IS25","IS22","IS21")
escolhidos <- c(Caso_min, 57, 54, 56)
# 
# escolhidos <- factor(x = c(Caso_min, 57, 54, 56), labels = c("Referência","IS25","IS22","IS21"))

# ParaGraf <- GerAnualporTipoTerm
# ParaGraf$Caso <- factor(GerAnualporTipoTerm$Caso, levels = c(Caso_min, 57, 54, 56), labels = c("Referência","IS25","IS22","IS21"))
# Soma das gerações de UHE e UTE por ano:
GerAnualAgrup <- group_by(BalancoTudo, Caso, Ano, Patamar, Subsistema) %>% filter(Energia == "Media") %>% summarise(Hid = mean(GeraHidrEnerg), Ter = mean(GeraTerEnerg)) %>% ungroup() %>% group_by(Caso, Ano) %>% summarise(HidrTot = sum(Hid), TermTot = sum(Ter))

GerAnualporTipo <- bind_rows(GerAnualporTipoTerm, rename(mutate(GerAnualAgrup, Tipo = "Hidro", Energia = "Media"), Gera = HidrTot))
GerAnualporTipo <- mutate(GerAnualporTipo, NomeCaso = factor(Caso, levels = escolhidos, labels = c("Referência","IS25","IS22","IS21")))
GerAnualporTipo$Tipo <- factor(GerAnualporTipo$Tipo, levels = c("UFV", "Eol", "CSP", "Hidro", "Bagaco", "Gas", "Carvao", "Nuclear", "OutrosExistentes"))

# Gráfico de área
ggplot(filter(GerAnualporTipo, Caso %in% escolhidos, Tipo %in% c("Carvao", "Gas", "Nuclear", "OutrosExistentes"), 
  Energia == "Media")) + geom_area(aes(x = Ano, y = Gera, fill = Tipo)) + facet_grid(~NomeCaso) + 
  scale_fill_brewer(palette = "BrBG") + ggtitle("Geração das usinas despacháveis") + xlab("") +
  ylab("Geração (MWano)") + labs(fill = "Fonte") + theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 16)) + 
  geom_vline(aes(xintercept = as.numeric(2022)), linetype = 4, colour = "black")
#Gráfico de coluna
ggplot(filter(GerAnualporTipo, Caso %in% escolhidos, Tipo %in% c("Carvao", "Gas", "Nuclear", "OutrosExistentes"), 
  Energia == "Media")) + geom_col(aes(x = Ano, y = Gera, fill = Tipo)) + facet_grid(~NomeCaso) + 
  scale_fill_brewer(palette = "BrBG") + ggtitle("Geração das usinas despacháveis") + xlab("") +
  ylab("Geração (MWano)") + labs(fill = "Fonte") + theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 16))

  #Gráfico só das usinas já existentes
ggplot(filter(GerAnualporTipo, Caso %in% escolhidos, Tipo %in% c("OutrosExistentes"), 
              Energia == "Media")) + geom_col(aes(x = Ano, y = Gera, fill = Tipo)) + facet_grid(~NomeCaso) + 
  scale_fill_brewer(palette = "BrBG") + ggtitle("Geração de todas as usinas") + xlab("") +
  ylab("Geração (MWano)") + labs(fill = "Fonte") + theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 16))

#Gráfico de todas as fontes
ggplot(filter(GerAnualporTipo, Caso %in% escolhidos, 
              Energia == "Media")) + geom_col(aes(x = Ano, y = Gera, fill = Tipo)) + facet_grid(~NomeCaso) + 
  scale_fill_brewer(palette = "BrBG") + ggtitle("Geração das usinas existentes") + xlab("") +
  ylab("Geração (MWano)") + labs(fill = "Fonte") + theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 16))

#Gráfico de despacháveis separadamente
ggplot(filter(GerAnualporTipo, Caso %in% escolhidos, Tipo %in% c("Carvao", "Gas", "Nuclear", "OutrosExistentes"),
              Energia == "Media")) + geom_col(aes(x = NomeCaso, y = Gera)) + facet_grid(Tipo ~ Ano) + 
  scale_fill_brewer(palette = "BrBG") + ggtitle("Geração de despacháveis") + xlab("") +
  ylab("Geração (MWano)") + labs(fill = "Fonte") + theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 16))

# ggplot(filter(GerAnualporTipoTerm, Tipo %in% c("Carvao", "Gas", "Nuclear", "OutrosExistentes"), 
#       Energia == "Media")) + geom_col(aes(x = Ano, y = Gera, fill = Tipo)) + facet_wrap(vars(Caso)) + scale_fill_brewer(palette = "BrBG")

