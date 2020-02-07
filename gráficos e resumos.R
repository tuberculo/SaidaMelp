
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
ggplot(filter(GerAnualporTipoTerm, Tipo %in% c("Carvao", "Gas", "Nuclear", "OutrosExistentes"), Energia == "Media")) + geom_col(aes(x = Ano, y = Gera, fill = Tipo)) + facet_wrap(vars(Caso))

# Soma das gerações de UHE e UTE por ano:
GerAnualporTipo <- group_by(BalancoTudo, Caso, Ano, Patamar, Subsistema) %>% filter(Energia == "Media") %>% summarise(Hid = mean(GeraHidrEnerg), Ter = mean(GeraTerEnerg)) %>% ungroup() %>% group_by(Caso, Ano) %>% summarise(HidrTot = sum(Hid), TermTot = sum(Ter))
