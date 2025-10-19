source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

rm(infos_clientes)
infos_clientes <- read_excel("relatorio_old_town_road.xlsx", sheet = "infos_clientes")
infos_clientes 
infos_clientes <- infos_clientes %>%
  rename(ClientID = Cli3ntID)

# convertendo altura e peso para as unidades corretas
infos_clientes <- infos_clientes %>%
  mutate(
    altura_cm = Height_dm * 10,       
    peso_kg = Weight_lbs * 0.453592       
  )
infos_clientes

# gerando os gráficos
grafico_dispersao <- ggplot(infos_clientes) +
  aes(x = peso_kg, y = altura_cm) +
  geom_point(colour = "#A11D21", size = 2, alpha = 0.4) +
  labs(
    x = "Peso (em quilogramas)",
    y = "Altura (em centímetros)"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
grafico_dispersao

histograma_peso <- ggplot(infos_clientes) +
  aes(x = peso_kg) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Peso (em quilogramas)", y = "Frequência
Absoluta") +
  theme_estat()
ggsave("hist_uni.pdf", width = 158, height = 93, units = "mm")
histograma_peso

histograma_altura <- ggplot(infos_clientes) +
  aes(x = altura_cm) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Altura (em centímetros)", y = "Frequência
Absoluta") +
  theme_estat()
ggsave("hist_uni.pdf", width = 158, height = 93, units = "mm")
histograma_altura

