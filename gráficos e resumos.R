library(tidyverse)

PatamarTudo <- readRDS("PatamarTudo.rds")
TermTudo <- readRDS("TermTudo.rds")
BalancoTudo <- readRDS("BalancoTudo.rds")


TermTudo <- mutate(TermTudo, Tipo = case_when(grepl("EOL", NOME) ~ "Eólica", 
                                              grepl("GAS", NOME) ~ "Gás",
                                              grepl("CARV", NOME) ~ "Carvão",
                                              grepl("SOL", NOME) ~ "UFV",
                                              grepl("CSP", NOME) ~ "CSP",
                                              grepl("BAG", NOME) ~ "Bagaço",
                                              grepl("NUC", NOME) ~ "Nuclear",
                                              grepl("ANGRA", NOME) ~ "Nuclear",
                                              TRUE ~ "OutrosExistentes"))

TermTudo <- mutate(TermTudo, NovaOuExistente = case_when(grepl("EOL", NOME) ~ "Nova", 
                                               grepl("GAS", NOME) ~ "Nova",
                                               grepl("CARV", NOME) ~ "Nova",
                                               grepl("SOL", NOME) ~ "Nova",
                                               grepl("CSP", NOME) ~ "Nova",
                                               grepl("BAG", NOME) ~ "Nova",
                                               grepl("NUC", NOME) ~ "Nova",
                                               grepl("ANGRA 3", NOME) ~ "Nova",
                                               TRUE ~ "Existente"))
GerAnualporTipoTerm <- group_by(TermTudo, Caso, Energia, Ano, Tipo, NovaOuExistente) %>% summarise(Gera = sum(GeraEner) / 12)

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
GerAnualporTipo$Tipo <- factor(GerAnualporTipo$Tipo, levels = c("UFV", "Eol", "CSP", "Hidro", "Bagaço", "Gás", "Carvão", "Nuclear", "OutrosExistentes"))

# Gráfico de área
ggplot(filter(GerAnualporTipo, Caso %in% escolhidos, Tipo %in% c("Carvão", "Gás", "Nuclear", "OutrosExistentes"), 
  Energia == "Media")) + geom_area(aes(x = Ano, y = Gera, fill = Tipo)) + facet_grid(~NomeCaso) + 
  scale_fill_brewer(palette = "BrBG") + ggtitle("Geração das usinas despacháveis") + xlab("") +
  ylab("Geração (MWano)") + labs(fill = "Fonte") + theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 16)) + 
  geom_vline(aes(xintercept = as.numeric(2022)), linetype = 4, colour = "black")
#Gráfico de coluna
ggplot(filter(GerAnualporTipo, Caso %in% escolhidos, Tipo %in% c("Carvão", "Gás", "Nuclear", "OutrosExistentes"), 
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
ggplot(filter(GerAnualporTipo, Caso %in% escolhidos, Tipo %in% c("Carvão", "Gás", "Nuclear", "OutrosExistentes"),
              Energia == "Media")) + geom_col(aes(x = NomeCaso, y = Gera)) + facet_grid(Tipo ~ Ano) + 
  scale_fill_brewer(palette = "BrBG") + ggtitle("Geração de despacháveis") + xlab("") +
  ylab("Geração (MWano)") + labs(fill = "Fonte") + theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size = 12))

#Gráfico de despacháveis "nova ou existente" até 2030
ggplot(filter(GerAnualporTipo, Caso %in% escolhidos, Tipo %in% c("Carvão", "Gás", "Nuclear", "OutrosExistentes"),
              Energia == "Media", Ano <= 2030, Ano >= 2020)) + geom_col(aes(x = NomeCaso, y = Gera)) + 
  facet_grid(NovaOuExistente ~ Ano) +   scale_fill_brewer(palette = "BrBG") + ggtitle("Geração de despacháveis") + xlab("") +
  ylab("Geração (MWano)") + labs(fill = "Fonte") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 16)) + theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 12))

# ggplot(filter(GerAnualporTipoTerm, Tipo %in% c("Carvao", "Gas", "Nuclear", "OutrosExistentes"), 
#       Energia == "Media")) + geom_col(aes(x = Ano, y = Gera, fill = Tipo)) + facet_wrap(vars(Caso)) + scale_fill_brewer(palette = "BrBG")

