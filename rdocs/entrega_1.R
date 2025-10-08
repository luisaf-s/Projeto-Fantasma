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
