# Data óbito ---------------------------------------------------------------

# por dia
da_dia_obito <- da_data_obito %>%
  dplyr::transmute(
    code_muni = as.character(code_muni),
    data = as.character(dt_alta_obito),
    srag, covid, covid_indef, covid_sintomas,
    obito, obito_covid
  ) %>%
  dplyr::full_join(dplyr::transmute(
    da_cams_media,
    code_muni = stringr::str_sub(code_muni, 1, 6),
    data = as.character(date),
    pm25
  ), by = c("data", "code_muni")
  ) %>%
  dplyr::transmute(
    code_muni,
    data = lubridate::ymd(data),
    ano = lubridate::year(data),
    mes = lubridate::month(data),
    srag, covid, covid_indef, covid_sintomas,
    obito, obito_covid,
    pm25,
    acima_25 = pm25 > 25
  ) %>%
  tidyr::replace_na(list(
    srag = 0, covid = 0, covid_indef = 0, covid_sintomas = 0,
    obito = 0, obito_covid = 0
  ))

da_mes_obito <- da_dia_obito %>%
  dplyr::group_by(code_muni, mes) %>%
  dplyr::summarise(
    srag = sum(srag),
    covid = sum(covid),
    covid_indef = sum(covid_indef),
    covid_sintomas = sum(covid_sintomas),
    obito = sum(obito),
    obito_covid = sum(obito_covid),
    pm25 = mean(pm25, na.rm = TRUE),
    dias_acima_25 = sum(acima_25),
    .groups = "drop"
  )

# tempo internação --------------------------------------------------------

da_srag_tempos <- da_srag_full %>%
  dplyr::filter(!obito) %>%
  dplyr::transmute(
    code_muni = co_mun_res,
    dt_exposicao,
    mes = lubridate::month(dt_exposicao),
    inicio = lubridate::ymd(dt_interna),
    alta = lubridate::dmy(dt_evoluca),
    encerramento = lubridate::dmy(dt_encerra),
    fim = dplyr::coalesce(alta, encerramento),
    tempo = fim - inicio,
    tempo = ifelse(tempo == 0, 0.01, tempo),
    mes_queimadas = ifelse(mes %in% 7:10, TRUE, FALSE)
  ) %>%
  dplyr::filter(!is.na(tempo), tempo >= 0, tempo <= 100) %>%
  dplyr::mutate(
    dt_exposicao = as.character(dt_exposicao),
    code_muni = as.character(code_muni)
  ) %>%
  dplyr::left_join(
    dplyr::transmute(
      da_cams_media,
      code_muni = stringr::str_sub(code_muni, 1, 6),
      dt_exposicao = as.character(date),
      pm25
    ), by = c("dt_exposicao", "code_muni")
  ) %>%
  dplyr::mutate(acima_25 = ifelse(pm25 > 25, TRUE, FALSE)) %>%
  dplyr::left_join(casos, c("code_muni", "mes"))

# tempo óbito -------------------------------------------------------------

da_srag_tempos_obito <- da_srag_full %>%
  dplyr::filter(obito) %>%
  dplyr::transmute(
    code_muni = co_mun_res,
    dt_exposicao,
    mes = lubridate::month(dt_exposicao),
    inicio = lubridate::ymd(dt_interna),
    alta = lubridate::dmy(dt_evoluca),
    encerramento = lubridate::dmy(dt_encerra),
    fim = dplyr::coalesce(alta, encerramento),
    tempo = fim - inicio,
    tempo = ifelse(tempo == 0, 0.01, tempo),
    mes_queimadas = ifelse(mes %in% 7:10, TRUE, FALSE)
  ) %>%
  dplyr::filter(!is.na(tempo), tempo >= 0, tempo <= 100) %>%
  dplyr::mutate(
    dt_exposicao = as.character(dt_exposicao),
    code_muni = as.character(code_muni)
  ) %>%
  dplyr::left_join(
    dplyr::transmute(
      da_cams_media,
      code_muni = stringr::str_sub(code_muni, 1, 6),
      dt_exposicao = as.character(date),
      pm25
    ), by = c("dt_exposicao", "code_muni")
  ) %>%
  dplyr::mutate(acima_25 = ifelse(pm25 > 25, TRUE, FALSE)) %>%
  dplyr::left_join(casos, c("code_muni", "mes"))


# readr::write_rds(da_srag_tempos, "data-tidy/da_srag_tempos.rds")
# readr::write_rds(da_srag_tempos_obito, "data-tidy/da_srag_tempos_obito.rds")
