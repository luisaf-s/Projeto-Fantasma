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
  aes(x = altura_cm, y = peso_kg) +
  geom_point(colour = "#A11D21", size = 2, alpha = 0.4) +
  labs(
    x = "Altura (em centímetros)",
    y = "Peso (em quilogramas)"
  ) +
  theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
grafico_dispersao

resumo_peso <- infos_clientes %>%
  summarise(
    clientes = n(),
    media = mean(peso_kg, na.rm = TRUE),
    mediana = median(peso_kg, na.rm = TRUE),
    Q1 = quantile(peso_kg, 0.25, na.rm = TRUE),
    Q3 = quantile(peso_kg, 0.75, na.rm = TRUE),
    min = min(peso_kg, na.rm = TRUE),
    max = max(peso_kg, na.rm = TRUE),
    variancia = var(peso_kg, na.rm = TRUE),
    desvio_padrao = sd(peso_kg, na.rm = TRUE)
  )

resumo_peso

resumo_altura <- infos_clientes %>%
  summarise(
    clientes = n(),
    media = mean(altura_cm, na.rm = TRUE),
    mediana = median(altura_cm, na.rm = TRUE),
    Q1 = quantile(altura_cm, 0.25, na.rm = TRUE),
    Q3 = quantile(altura_cm, 0.75, na.rm = TRUE),
    min = min(altura_cm, na.rm = TRUE),
    max = max(altura_cm, na.rm = TRUE),
    variancia = var(altura_cm, na.rm = TRUE),
    desvio_padrao = sd(altura_cm, na.rm = TRUE)
  )
resumo_altura

pearson <- cor(infos_clientes$peso_kg,infos_clientes$altura_cm)
pearson
