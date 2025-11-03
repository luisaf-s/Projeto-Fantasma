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

infos_vendas <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_vendas")
infos_produtos <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_produtos")
infos_funcionarios <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_funcionarios")
infos_cidades <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_cidades")
infos_clientes <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_clientes")
infos_lojas <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_lojas")
infos_cidades <- infos_cidades %>%
  rename(CityID = C1tyID)
infos_clientes <- infos_clientes %>%
  rename(ClientID = Cli3ntID)
infos_cidades
relatorio_vendas <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "relatorio_vendas")
relatorio_final <- inner_join(infos_cidades, infos_lojas, by = "CityID") 
relatorio_final <- relatorio_final %>%
  rename(StoreID = Stor3ID)
relatorio_final <- inner_join(relatorio_final, relatorio_vendas, by = "StoreID")
relatorio_final <- inner_join(relatorio_final, infos_clientes, by = "ClientID")
relatorio_final
cidade <- relatorio_final %>%
  filter(NameCity == "Âmbar Seco")

boxplot <- ggplot(cidade) +
  aes(x = reorder(NameStore, Age, FUN = median), y = Age) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Loja", y = "Idade dos clientes") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")
boxplot


resumo_idade <- cidade %>%
  distinct(ClientID, .keep_all = T) %>%
  group_by(NameStore) %>%
  summarise(
    clientes = n(),
    media = mean(Age, na.rm = TRUE),
    mediana = median(Age, na.rm = TRUE),
    Q1 = quantile(Age, 0.25, na.rm = TRUE),
    Q3 = quantile(Age, 0.75, na.rm = TRUE),
    min = min(Age, na.rm = TRUE),
    max = max(Age, na.rm = TRUE),
    variancia = var(Age, na.rm = TRUE),
    desvio_padrao = sd(Age, na.rm = TRUE)
  )

resumo_idade


