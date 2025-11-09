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
rm(vendas_media_ano)


real <- vendas_ano %>%
  group_by(Date) %>%
  reframe(PreçoTotal = ((UnityPrice * Quantity)*5.31)) 

media_por_ano <- real %>%
  group_by(Date) %>%
  summarise(media_real = sum(PreçoTotal)/18)

grafico_linhas <- ggplot(media_por_ano) +
  aes(x=Date, y=media_real, group=1) +
  geom_line(linewidth=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Receita média em reais") +
  scale_x_continuous(breaks = 1880:1889, limits = c(1880,1889))
theme_estat()
ggsave("series_uni.pdf", width = 158, height = 93, units = "mm")

grafico_linhas2 <- ggplot(media_por_ano) +
  aes(Date, media_real, group=1) +
  geom_line(linewidth=1,colour="#A11D21") + 
  geom_point(colour="#A11D21", size=2) +
  labs(x="Ano", y="Receita Média em Reais") +
  theme_estat() +
  scale_x_continuous(breaks = scales::pretty_breaks(n =10))

grafico_linhas
grafico_linhas2

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
vendas_media_ano<- inner_join(vendas_ano, infos_cidades, by="CityID")

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
print_quadro_resumo <- function(data, var_name, title = "Medidas resumo da(o) [nome da variável]", label = "quad:quadro_resumo1") {
  var_name <- substitute(var_name)
  
  data <- data %>%
    group_by(group_var = data[[deparse(var_name)]]) %>%
    summarize(
      Média = round(mean(!!sym(deparse(var_name))), 2),
      "Desvio Padrão" = round(sd(!!sym(deparse(var_name))), 2),
      Variância = round(var(!!sym(deparse(var_name))), 2),
      Mínimo = round(min(!!sym(deparse(var_name))), 2),
      "1º Quartil" = round(quantile(!!sym(deparse(var_name)), probs = .25), 2),
      Mediana = round(quantile(!!sym(deparse(var_name)), probs = .5), 2),
      "3º Quartil" = round(quantile(!!sym(deparse(var_name)), probs = .75), 2),
      Máximo = round(max(!!sym(deparse(var_name))), 2)
    ) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep = "")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  
  latex <- str_c(latex, "| l |\n", sep = " ")
  for (i in seq(2, col_count)) {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount, ".2]\n", sep = "")
  }
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep = "")
  
  if (col_count > 2) {
    for (i in seq(1, col_count)) {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep = "")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep = "")
      if (i < col_count)
        latex <- str_c(latex, "&", sep = " ")
      else
        latex <- str_c(latex, "\\\\\n", sep = " ")
    }
  } else {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep = "")
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep = "")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count)) {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep = "")
  
  writeLines(latex)
}


vendas_media_ano 

vendas_1889 <- vendas_media_ano %>%
  filter(Date == 1889) %>%
  group_by(NameStore) %>%
  summarise(total = sum(UnityPrice*Quantity)*5.31)%>%
  arrange(desc(total)) %>%
  slice_head(n = 3)
vendas_1889


grafico_maiores <- ggplot(vendas_1889) +
  aes(x = NameStore, y = total) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  labs(x = "Lojas", y = "Faturamento") +
  theme_estat()
grafico_maiores
rm(maiores)
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm"
)

OuroFino <- vendas_media_ano %>%
  filter(NameStore == "Loja Ouro Fino", Date == 1889) %>%
  group_by(NameProduct) %>%
  summarise(quantidade = sum(Quantity)) %>%
  arrange(desc(quantidade)) %>%
  slice_head(n = 3)

TendTudo <- vendas_media_ano %>%
  filter(NameStore == "Loja TendTudo", Date == 1889) %>%
  group_by(NameProduct) %>%
  summarise(quantidade = sum(Quantity)) %>%
  arrange(desc(quantidade)) %>%
  slice_head(n = 3)

Ferraria <- vendas_media_ano %>%
  filter(NameStore == "Ferraria Apache", Date == 1889) %>%
  group_by(NameProduct) %>%
  summarise(quantidade = sum(Quantity)) %>%
  arrange(desc(quantidade)) %>%
  slice_head(n = 4)

grafico_ourofino <- ggplot(OuroFino) +
  aes(x = NameProduct, y = quantidade) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  labs(x = "Nome do Produto", y = "Quantidade") +
  theme_estat()
grafico_ourofino
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm"
)

grafico_tendtudo <- ggplot(TendTudo) +
  aes(x = NameProduct, y = quantidade) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  labs(x = "Nome do Produto", y = "Quantidade") +
  theme_estat()
grafico_tendtudo
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm"
)

grafico_ferraria <- ggplot(Ferraria) +
  aes(x = NameProduct, y = quantidade) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  labs(x = "Nome do Produto", y = "Quantidade") +
  theme_estat()
grafico_ferraria
ggsave("colunas-uni-freq.pdf", width = 158, height = 93, units = "mm"
)

lojas <- data.frame(
  Loja = c(rep("Ouro Fino", 3), rep("TendTudo", 3), rep("Ferraria Apache", 4)),
  Produto = c("Botas de Couro", "Whisky", "Chapéu de Couro", "Espingarda", "Whisky", "Colt.45", "Chapéu de Couro", "Espingarda", "Machado", "Whisky"),
  Quantidade = c(52, 49, 45, 53, 49, 43, 52, 42, 41, 41))

colunas_bivariado <- ggplot(lojas, aes(x = Loja, y = Quantidade, fill = Produto)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Quantidade),
            position = position_dodge(width = 0.9),
            vjust = -0.5, hjust = 0.5, size = 3.5) +
  labs(x = "Lojas", y = "Quantidade", fill = "Produto") +
  theme_estat()
colunas_bivariado


