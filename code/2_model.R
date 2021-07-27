library(magrittr)
library(glmmTMB)
library(DHARMa)

# import ------------------------------------------------------------------

da_model <- readr::read_rds("data-tidy/base_final.rds")

# distribuição da resposta internações em escala log
da_model %>%
  ggplot2::ggplot(ggplot2::aes(x = covid)) +
  ggplot2::geom_histogram() +
  ggplot2::scale_x_log10()

# compara obitos por covid sivep x brasil.io
da_model %>%
  dplyr::group_by(uf, mes) %>%
  dplyr::summarise(
    obito_covid = sum(obito_covid),
    mortes_covid_mes = sum(mortes_covid_mes),
    .groups = "drop"
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = mes, y = obito_covid, group = uf)) +
  ggplot2::geom_line(ggplot2::aes(x = mes, y = mortes_covid_mes, group = uf), linetype = 2) +
  ggplot2::facet_wrap(~uf) +
  ggplot2::scale_x_continuous(breaks = 1:12) +
  ggplot2::labs(y = "Total óbitos por UF", x = "mês")


# Modelo SRAG -------------------------------------------------------------
# Resp: srag
# Fixos: pop, pm25, precipitacao, dias_acima_25, uf, porte, leitos
# Aleatórios: ano_mes:code_muni
# Dist: nbinom2
fit_int_srag <- glmmTMB(
  srag ~ offset(log(pop)) + pm25 + precipitacao +
    casos_covid_mes +
    dias_acima_25 + uf + porte + area_mun_km +
    (1|code_muni:ano_mes),
  data = da_model, family = nbinom2
)
summary(fit_int_srag)
res <- simulateResiduals(fit_int_srag)
plot(res)
readr::write_rds(fit_int_srag, "outputs/modelos/modelo_internacoes_srag.rds")
readr::write_rds(fit_int_srag, "outputs/app/internacoes/data/modelo_internacoes_srag.rds")

# Modelo Covid ------------------------------------------------------------
# Resp: covid
# Fixos: pop, pm25, precipitacao, dias_acima_25, uf, porte, leitos
# Aleatórios: ano_mes:code_muni
# Dist: nbinom2
fit_int_covid <- glmmTMB(
  covid ~ offset(log(pop)) + pm25 + precipitacao +
    casos_covid_mes +
    dias_acima_25 + uf + porte + area_mun_km +
    (1|code_muni:ano_mes),
  data = da_model, family = nbinom2
)
summary(fit_int_covid)
res <- simulateResiduals(fit_int_covid)
plot(res)
readr::write_rds(fit_int_covid, "outputs/modelos/modelo_internacoes_covid.rds")
readr::write_rds(fit_int_covid, "outputs/app/internacoes/data/modelo_internacoes_covid.rds")

# Modelo óbitos covid -----------------------------------------------------
# Resp: obito_covid
# Fixos: pop, pm25, precipitacao, dias_acima_25, uf, porte, leitos
# Aleatórios: ano_mes:code_muni
# Dist: nbinom2
fit_ob_covid <- glmmTMB(
  mortes_covid_mes ~ offset(log(pop)) + precipitacao +
    casos_covid_mes + capital +
    uf +
    dias_acima_25 + porte +
    area_mun_km +
    (1|ano_mes:code_muni),
  data = da_model, family = nbinom2,
  ziformula = ~1
)
summary(fit_ob_covid)
res <- simulateResiduals(fit_ob_covid)
plot(res)
readr::write_rds(fit_ob_covid, "outputs/modelos/modelo_obitos_covid.rds")
readr::write_rds(fit_ob_covid, "outputs/app/internacoes/data/modelo_obitos_covid.rds")


# Modelo óbitos SRAG -----------------------------------------------------
# Resp: obito
# Fixos: pop, pm25, precipitacao, dias_acima_25, uf, porte, leitos
# Aleatórios: ano_mes:code_muni
# Dist: nbinom2
fit_ob_srag <- glmmTMB(
  obito ~ offset(log(pop)) + precipitacao +
    # casos_covid_mes + capital +
    uf +
    dias_acima_25 + pm25 +
    porte + area_mun_km +
    (1|ano_mes:code_muni),
  data = da_model, family = nbinom2,
  ziformula = ~1
)
summary(fit_ob_srag)
res <- simulateResiduals(fit_ob_srag)
plot(res)
readr::write_rds(fit_ob_srag, "outputs/modelos/modelo_obitos_srag.rds")
readr::write_rds(fit_ob_srag, "outputs/app/internacoes/data/modelo_obitos_srag.rds")


# Modelo óbitos covid (considerando data de óbito) ------------------------
# da_mes_obito

fit_ob_covid_datasus <- glmmTMB(
  obito_covid ~ offset(log(pop)) + precipitacao +
    casos_covid_mes + capital +
    uf +
    dias_acima_25 + porte +
    area_mun_km +
    (1|ano_mes:code_muni),
  data = da_model, family = nbinom2,
  ziformula = ~1
)
summary(fit_ob_covid_datasus)
res <- simulateResiduals(fit_ob_covid_datasus)
plot(res)


# Modelo óbitos covid (dados brasil.io defasados) -------------------------

fit_ob_covid <- glmmTMB(
  mortes_covid_mes ~ offset(log(pop)) + precipitacao +
    casos_covid_mes + capital +
    uf +
    dias_acima_25 + porte +
    area_mun_km +
    (1|ano_mes:code_muni),
  data = da_model, family = nbinom2
  # ziformula = ~1
)
summary(fit_ob_covid)
res <- simulateResiduals(fit_ob_covid)
plot(res)


# 1. comparar óbitos e tempo de internação dos pacientes que já tinham cardiopatia,
# diabetes, obesidade, asma ou pneumopatia, com os que não tinham esses fatores de risco

# 2. comparar óbitos e tempo de internação dos pacientes com 60+ , em relação às
# outras idades





da_model %>%
  dplyr::transmute(
    code_muni, muni_nm, uf, pop, mes,
    internacoes_srag = srag,
    internacoes_covid = covid,
    dias_acima_25
  ) %>%
  dplyr::mutate(
    pct_int_srag = exp(ef_int_srag$dias_acima_25)^dias_acima_25,
    pct_int_covid = exp(ef_int_covid$dias_acima_25)^dias_acima_25,
    srag_hipotetico = internacoes_srag/pct_int_srag,
    covid_hipotetico = internacoes_covid/pct_int_srag,
    porte = dplyr::case_when(
      pop < 25000 ~ "p",
      pop < 100000 ~ "m",
      TRUE ~ "g"
    )
  ) %>%
  dplyr::relocate(porte, .after = pop) %>%
  readr::write_csv("outputs/csv/internacoes_srag_covid.csv")
