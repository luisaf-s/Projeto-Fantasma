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

# carregando os dados

excel <- read_xlsx("relatorio_old_town_road.xlsx")
relatorio_vendas <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "relatorio_vendas")
infos_vendas <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_vendas")
infos_produtos <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_produtos")
infos_funcionarios <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_funcionarios")
infos_cidades <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_cidades")
infos_clientes <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_clientes")
infos_lojas <- read_xlsx("relatorio_old_town_road.xlsx", sheet = "infos_lojas")
infos_vendas

# juntar as tabelas das variáveis

infos_vendas <- infos_vendas %>% 
  rename(SaleID = Sal3ID)
infos_vendas
infos_produtos <- infos_produtos %>%
  rename(ItemID = Ite3ID)
infos_produtos
infos_lojas <- infos_lojas %>%
  rename(StoreID = Stor3ID)
infos_lojas

vendas_completo <- left_join(relatorio_vendas, infos_vendas, by = "SaleID")
vendas_completo <- left_join(vendas_completo, infos_produtos, by = "ItemID")
vendas_completo <- left_join(vendas_completo,infos_lojas, by = "StoreID")
vendas_completo

vendas_ano <- vendas_completo %>%
  mutate(Date = year(Date)) 
  
real <- vendas_ano %>%
  group_by(Date) %>%
  reframe(PreçoTotal = ((UnityPrice * Quantity)*5.31)) 

media_por_ano <- real %>%
  group_by(Date) %>%
  summarise(media_real = mean(PreçoTotal))

grafico <- ggplot(media_por_ano) +
  aes(x = Date, y = media_real) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Anos",
    y = "Média"
  ) +
  theme_estat() + 
  scale_x_continuous(limits = c(1880,1889), breaks = seq(1880,1889,1))
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")

grafico



  
