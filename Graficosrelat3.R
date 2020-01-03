source("C:/Users/Rafael/Google Drive/SINAPSE/Melp/Scripts/Lerelat3.R")

# Graficos: Separar em outro script depois. Usar o "source" p/ chamar o script Lerelat3
# Potencia instalada por subsistema, fonte e ano
graf_pot_inst <- relat3 %>% filter(Tipo != "Intr.") %>% group_by(Ano,Subsistema, Tipo) %>% 
  summarise(Pot = sum(Pot.Ins.)) %>% ungroup()
ggplot(data = graf_pot_inst, aes(x = Ano, y = Pot, fill = Tipo)) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette= "Set2") +
  facet_wrap(~Subsistema, nrow = 2) + ggtitle("Potência Instalada") +
  xlab("") + ylab("Potência (MW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

# Energia media e critica
graf_E_crit_inst <- relat3 %>% filter(Tipo != "Intr.") %>% 
  group_by(Ano,Subsistema, Tipo) %>% summarise(E_crit = sum(`E. Crit.`)) %>% 
  mutate(Energia = "Critica") %>% ungroup() 
graf_E_med_inst <- relat3 %>% filter(Tipo != "Intr.") %>% 
  group_by(Ano,Subsistema, Tipo) %>% summarise(E_crit = sum(`E. Med.`)) %>%
  mutate(Energia = "Media") %>% ungroup() 
graf_cenarios <- bind_rows(graf_E_crit_inst,graf_E_med_inst)
# En crítica
ggplot(data = graf_cenarios %>% filter(Energia == "Critica"), 
       aes(x = Ano, y = E_crit, fill = Tipo)) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette= "Set2") +
  facet_wrap(~Subsistema, nrow = 2) + ggtitle("Energia crítica") +
  xlab("") + ylab("Energia (MWmed)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))
# En média
ggplot(data = graf_cenarios %>% filter(Energia == "Media"), 
       aes(x = Ano, y = E_crit, fill = Tipo)) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette= "Set2") +
  facet_wrap(~Subsistema,nrow = 2) + ggtitle("Energia média") +
  xlab("") + ylab("Energia (MWmed)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))
# Tentando colocar as duas no mesmo gráfico: não funcionando por enquanto
ggplot(data = graf_cenarios,aes(x = Ano, y = E_crit, group = Tipo, fill = Energia)) + 
  geom_bar(stat = "identity", position = "stack") + scale_fill_brewer(palette= "Set2") +
  facet_wrap(~Subsitema, nrow = 2) + ggtitle("Energia") +
  xlab("") + ylab("Energia (MWmed)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

# To DO: 
# Um gráfico com en média, firme e potência (3 quadros)
# 2) Pegar os custos (investimento, operação e total) do relat3
# 3) Diferenciar as fontes termicas
# 4) Partiu relat10!! :)

