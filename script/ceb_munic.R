
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(basedosdados)
library(patchwork)

# Definindo projeto para acesso aos dados ---------------------------------

set_billing_id("consulta-jan-2023")

# Indicadores educacionais - Itabira --------------------------------------

# Definicao de parametros de busca

query_ind_educ <-
  c("SELECT ano, id_municipio, localizacao, rede, atu_em, tdi_ef,
  tdi_em, taxa_aprovacao_ef, taxa_aprovacao_em, taxa_abandono_ef_anos_iniciais,
  taxa_abandono_ef_anos_finais, taxa_abandono_em,
    FROM `basedosdados.br_inep_indicadores_educacionais.municipio`
    WHERE id_municipio = '3131703' and ano > 2010")

# importacao dos parametros
ind_educ <- read_sql(query_ind_educ)


# Analises ----------------------------------------------------------------

ind_educ_1 <- ind_educ |>
  select(-id_municipio) |>
  mutate(localizacao = as.factor(localizacao),
         rede = as.factor(rede))
# Media de alunos no ensino médio por tipo de rede

ind_educ_1 |>
  filter(rede == "publica" | rede == "privada") |>
  filter(localizacao == "total") |>
  ggplot() +
  aes(x = as.factor(ano), y = atu_em, group = rede, fill = rede) +
  geom_col(stat = "summary",
           position="dodge") +
  theme_light() +
  scale_y_continuous(breaks = seq(0,50,5)) +
  labs(
    title = "Média de alunos por turma de Ensino Médio - Itabira (MG), 2011-2020",
    subtitle = "Por tipo de rede de ensino (pública ou privada)",
    x = "Ano",
    y = "Média de alunos por turma",
    caption = "INEP, Indicadores educacionais (2011-2020). Elaborado por @thiagocalm.") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "bold", size = 14),
    plot.caption = element_text(face = "italic", size = 8),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10, colour = "white")
  ) +
  scale_fill_brewer(palette = "Dark2")

# Taxa de aprovação EF

tx_aprov_ef_graf <- ind_educ_1 |>
  filter(rede == "publica" | rede == "privada") |>
  filter(localizacao == "total") |>
  filter(ano <= 2019) |>
  ggplot() +
  aes(x = as.factor(ano), y = taxa_aprovacao_ef, group = rede, fill = rede) +
  geom_col(stat = "summary",
           position="dodge") +
  geom_hline(yintercept = 100, colour = "black", linetype = "dashed", size = 1.2) +
  theme_light() +
  scale_y_continuous(breaks = seq(0,100,10)) +
  labs(
    title = "Taxa de aprovação do (i) Ensino Fundamental e (ii) Ensino Médio \n por tipo de rede de ensino (pública ou privada) - Itabira (MG), 2011-2019",
    subtitle = "(i) Ensino Fundamental",
    x = "Ano",
    y = "Taxa de aprovação EF") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = .5),
    plot.subtitle = element_text(face = "italic", size = 12, hjust = .5),
    # plot.caption = element_text(face = "italic", size = 8),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10, colour = "white")
  ) +
  scale_fill_brewer(palette = "Dark2")

# Taxa de aprovação EM

tx_aprov_em_graf <- ind_educ_1 |>
  filter(rede == "publica" | rede == "privada") |>
  filter(localizacao == "total") |>
  filter(ano <= 2019) |>
  ggplot() +
  aes(x = as.factor(ano), y = taxa_aprovacao_em, group = rede, fill = rede) +
  geom_col(stat = "summary",
           position="dodge") +
  geom_hline(yintercept = 100, colour = "black", linetype = "dashed", size = 1.2) +
  theme_light() +
  scale_y_continuous(breaks = seq(0,100,10)) +
  labs(
    subtitle = "(ii) Ensino Médio",
    x = "Ano",
    y = "Taxa de aprovação EM",
    caption = "INEP, Indicadores educacionais (2011-2019). Elaborado por @thiagocalm.") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.subtitle = element_text(face = "italic", size = 12, hjust = .5),
    plot.caption = element_text(face = "italic", size = 8),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10, colour = "white")
  ) +
  scale_fill_brewer(palette = "Dark2")

tx_aprov_ef_graf/tx_aprov_em_graf

# Taxa de abandono EF

tx_abandono_ef_graf <- ind_educ_1 |>
  filter(rede == "publica" | rede == "privada") |>
  filter(localizacao == "total") |>
  filter(ano <= 2019) |>
  ggplot() +
  aes(x = as.factor(ano), y = taxa_abandono_ef_anos_finais, group = rede, fill = rede) +
  geom_col(stat = "summary",
           position="dodge") +
  theme_light() +
  labs(
    title = "Taxa de abandono do (i) Ensino Fundamental anos finais e (ii) Ensino Médio \n por tipo de rede de ensino (pública ou privada) - Itabira (MG), 2011-2019",
    subtitle = "(i) Ensino Fundamental (anos finais)",
    x = "Ano",
    y = "Taxa de abandono EF") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = .5),
    plot.subtitle = element_text(face = "italic", size = 12, hjust = .5),
    # plot.caption = element_text(face = "italic", size = 8),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10, colour = "white")
  ) +
  scale_fill_brewer(palette = "Dark2")

# Taxa de abandono EM

tx_abandono_em_graf <- ind_educ_1 |>
  filter(rede == "publica" | rede == "privada") |>
  filter(localizacao == "total") |>
  filter(ano <= 2019) |>
  ggplot() +
  aes(x = as.factor(ano), y = taxa_abandono_em, group = rede, fill = rede) +
  geom_col(stat = "summary",
           position="dodge") +
  theme_light() +
  labs(
    subtitle = "(ii) Ensino Médio",
    x = "Ano",
    y = "Taxa de abandono EM",
    caption = "INEP, Indicadores educacionais (2011-2019). Elaborado por @thiagocalm.") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.subtitle = element_text(face = "italic", size = 12, hjust = .5),
    plot.caption = element_text(face = "italic", size = 8),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10, colour = "white")
  ) +
  scale_fill_brewer(palette = "Dark2")

tx_abandono_ef_graf/tx_abandono_em_graf
