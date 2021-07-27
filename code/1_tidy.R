library(magrittr)

tirar_var <- c(
  "co_ps_vgm", "cod_idade", "dt_rt_vgm", "dt_vgm",
  "obes_imc",  "pac_cocbo", "pac_dscbo", "pais_vgm"
)

da_srag_full <- "data-raw/srag_INFLUD-24-05-2021.csv" %>%
  data.table::fread() %>%
  janitor::clean_names() %>%
  dplyr::select(-dplyr::all_of(tirar_var))

da_cams_municipio <- readr::read_csv("data-raw/pm25_muni_daily.csv") %>%
  dplyr::filter(!is.na(ppm25)) %>%
  dplyr::rename(pm25 = ppm25) %>%
  dplyr::mutate(pm25 = as.numeric(pm25))

mun_amazonia <- unique(da_cams_municipio$code_muni)
mun_amazonia6 <- stringr::str_sub(mun_amazonia, 1, 6)

# Tidy SRAG --------------------------------------------------------------------

da_srag_full <- da_srag_full %>%
  dplyr::filter(co_mun_res %in% mun_amazonia6) %>%
  dplyr::mutate(
    dt_interna = lubridate::dmy(dt_interna),
    dt_alta_obito = lubridate::dmy(dt_evoluca),
    # lag 7 dias
    dt_exposicao = dt_interna - 7,
    indef = ifelse(classi_fin %in% 1:3, FALSE, TRUE),
    apenas_covid = ifelse(classi_fin != 5 | is.na(classi_fin), FALSE, TRUE),
    tosse = ifelse(tosse == 1 & !is.na(tosse), TRUE, FALSE),
    garganta = ifelse(garganta == 1 & !is.na(garganta), TRUE, FALSE),
    desc_resp = ifelse(desc_resp == 1 & !is.na(desc_resp), TRUE, FALSE),
    saturacao = ifelse(saturacao == 1 & !is.na(saturacao), TRUE, FALSE),
    perd_olft = ifelse(perd_olft == 1 & !is.na(perd_olft), TRUE, FALSE),
    perd_pala = ifelse(perd_pala == 1 & !is.na(perd_pala), TRUE, FALSE),
    sintomas = tosse + garganta + desc_resp + saturacao + perd_olft + perd_pala,
    febre = ifelse(febre != 1 | is.na(febre), FALSE, TRUE),
    covid_sintomas = apenas_covid | (indef & febre & sintomas > 0),
    covid_sintomas = covid_sintomas & (pos_pcrout != 2 | is.na(pos_pcrout)),
    obito = ifelse(evolucao == 2 & !is.na(evolucao), TRUE, FALSE),
    obito_covid = obito & apenas_covid
  ) %>%
  dplyr::filter(dt_exposicao >= "2020-01-01", dt_exposicao <= "2020-12-31")

da_srag_cat <- da_srag_full %>%
  dplyr::transmute(
    code_muni = co_mun_res,
    dt_exposicao,
    apenas_covid,
    indef,
    covid_sintomas,
    obito,
    obito_covid
  ) %>%
  dplyr::group_by(code_muni, dt_exposicao) %>%
  dplyr::summarise(
    srag = dplyr::n(),
    covid = sum(apenas_covid),
    covid_indef = sum(indef),
    covid_sintomas = sum(covid_sintomas),
    obito = sum(obito),
    obito_covid = sum(obito_covid),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    code_muni, dt_exposicao = tidyr::full_seq(dt_exposicao, 1),
    fill = list(
      srag = 0, covid = 0, covid_indef = 0, covid_sintomas = 0,
      obito = 0, obito_covid = 0
    )
  ) %>%
  dplyr::ungroup()
readr::write_rds(da_srag_cat, "data-tidy/srag_cat.rds")


# Data óbito --------------------------------------------------------------

da_data_obito <- da_srag_full %>%
  dplyr::transmute(
    code_muni = co_mun_res,
    dt_alta_obito,
    apenas_covid,
    indef,
    covid_sintomas,
    obito,
    obito_covid
  ) %>%
  dplyr::group_by(code_muni, dt_alta_obito) %>%
  dplyr::summarise(
    srag = dplyr::n(),
    covid = sum(apenas_covid),
    covid_indef = sum(indef),
    covid_sintomas = sum(covid_sintomas),
    obito = sum(obito),
    obito_covid = sum(obito_covid),
    .groups = "drop"
  ) %>%
  tidyr::complete(
    code_muni, dt_alta_obito = seq(lubridate::ymd("2020-01-01"), lubridate::ymd("2020-12-31"), 1),
    fill = list(
      srag = 0, covid = 0, covid_indef = 0, covid_sintomas = 0,
      obito = 0, obito_covid = 0
    )
  )

# Tidy CAMS --------------------------------------------------------------------

# pm25 médio de regiões urbanas por município/dia
da_cams_media <- da_cams_municipio %>%
  dplyr::filter(fun == "mean") %>%
  dplyr::transmute(
    code_muni = stringr::str_sub(code_muni, 1, 6),
    date, pm25
  )
# readr::write_rds(da_cams_media, "data-tidy/cams_media.rds")

da_cams_mes <- da_cams_media %>%
  dplyr::mutate(
    mes = lubridate::month(date),
    ano = lubridate::year(date),
    code_muni = stringr::str_sub(code_muni, 1, 6)
  ) %>%
  dplyr::group_by(code_muni, mes, ano) %>%
  dplyr::summarise(pm25_mediamensal = mean(pm25), .groups = "drop")

# Join Covid + CAMS --------------------------------------------------------

# por dia
da_dia <- da_srag_cat %>%
  dplyr::transmute(
    code_muni = as.character(code_muni),
    data = as.character(dt_exposicao),
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

da_mes <- da_dia %>%
  dplyr::group_by(code_muni, mes) %>%
  dplyr::summarise(
    srag = sum(srag),
    covid = sum(covid),
    covid_indef = sum(covid_indef),
    covid_sintomas = sum(covid_sintomas),
    obito = sum(obito),
    obito_covid = sum(obito_covid),
    pm25 = mean(pm25),
    dias_acima_25 = sum(acima_25),
    .groups = "drop"
  )



# Capital -----------------------------------------------------------------

capitais <- c(
  "120040", "160030", "130260", "510340", "150140",
  "110020", "140010", "172100", "211130"
)

capital <- tibble::tibble(
  code_muni = unique(da_mes$code_muni)
) %>%
  dplyr::mutate(
    capital = ifelse(code_muni %in% capitais, TRUE, FALSE),
    capital = as.factor(capital)
  )


# PNUD -------------------------------------------------------------------

pnud <- abjData::pnud_min %>%
  dplyr::filter(ano == "2010") %>%
  dplyr::mutate(muni_id = stringr::str_sub(muni_id, 1, 6)) %>%
  dplyr::transmute(
    code_muni = muni_id,
    muni_nm,
    uf = uf_sigla,
    idhm,
    espvida,
    rdpc,
    gini,
    pop,
    porte = as.factor(dplyr::case_when(
      pop < 25000 ~ "p",
      pop < 100000 ~ "m",
      TRUE ~ "g"
    ))
  )

# Dados focos de calor ----------------------------------------------------

fogo <- readr::read_rds("data-raw/fire_muni_monthly.rds")
fogo <- fogo %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(code_muni)) %>%
  dplyr::transmute(
    code_muni = stringr::str_sub(code_muni, 1, 6),
    mes, n_focos,
    dens_focos
  ) %>%
  tidyr::complete(
    code_muni, mes = 1:12,
    fill = list(n_focos = 0, dens_focos = 0)
  )

# Desmatamento ------------------------------------------------------------

desmatamento <- readr::read_csv("data-raw/deter-amz-daily-5-27-2021-12 09 36 AM.csv")

pnud_rm <- pnud %>%
  dplyr::mutate(muni_nm = abjutils::rm_accent(muni_nm))

desmatamento <- desmatamento %>%
  janitor::clean_names() %>%
  dplyr::transmute(
    data = lubridate::mdy(view_date),
    mes = lubridate::month(data),
    uf,
    muni_nm = toupper(municipio),
    area_mun_km, area_uc_km
  ) %>%
  dplyr::group_by(muni_nm, uf, mes) %>%
  dplyr::summarise(area_mun_km = sum(area_mun_km), .groups = "drop") %>%
  tidyr::unite(muni_nm_uf, muni_nm, uf, sep = ";", remove = FALSE) %>%
  tidyr::complete(
    muni_nm_uf, mes = 1:12,
    fill = list(area_mun_km = 0)
  ) %>%
  tidyr::separate(muni_nm_uf, c("muni_nm", "uf"), sep = ";") %>%
  dplyr::mutate(muni_nm = abjutils::rm_accent(muni_nm)) %>%
  dplyr::inner_join(pnud_rm, c("muni_nm", "uf")) %>%
  dplyr::select(code_muni, mes, area_mun_km)

# Casos COVID -------------------------------------------------------------

casos <- readr::read_csv("data-raw/caso.csv.gz")

casos <- casos %>%
  dplyr::mutate(date = date - 14) %>%
  dplyr::filter(
    city_ibge_code %in% mun_amazonia,
    dplyr::between(date, as.Date("2020-01-01"), as.Date("2020-12-31"))
  ) %>%
  dplyr::transmute(
    code_muni = stringr::str_sub(city_ibge_code, 1, 6),
    mes = lubridate::month(date),
    confirmed,
    deaths
  ) %>%
  dplyr::arrange(code_muni, mes) %>%
  dplyr::group_by(code_muni, mes) %>%
  dplyr::summarise(
    casos_covid = dplyr::last(confirmed),
    mortes_covid = dplyr::last(deaths), .groups = "drop"
  ) %>%
  tidyr::complete(
    code_muni, mes = 1:12
  ) %>%
  dplyr::mutate(
    casos_covid = ifelse(mes == 1, 0, casos_covid),
    mortes_covid = ifelse(mes == 1, 0, mortes_covid)
  ) %>%
  tidyr::fill(casos_covid, mortes_covid) %>%
  dplyr::group_by(code_muni) %>%
  dplyr::transmute(
    mes,
    casos_covid_acum = casos_covid,
    casos_covid_mes = casos_covid_acum - dplyr::lag(casos_covid_acum),
    mortes_covid_acum = mortes_covid,
    mortes_covid_mes = mortes_covid_acum - dplyr::lag(mortes_covid_acum)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::replace_na(list(casos_covid_mes = 0, mortes_covid_mes = 0)) %>%
  dplyr::mutate(
    casos_covid_mes = ifelse(casos_covid_mes < 0, 0, casos_covid_mes),
    mortes_covid_mes = ifelse(mortes_covid_mes < 0, 0, mortes_covid_mes)
  )


# Precipitação ------------------------------------------------------------

precip <- readr::read_csv("data-raw/rain_muni_monthly.csv")
precip <- precip %>%
  janitor::clean_names() %>%
  dplyr::select(
    code,
    x0_precipitation:x9_precipitation
  ) %>%
  tidyr::pivot_longer(
    x0_precipitation:x9_precipitation,
    names_to = "mes", values_to = "precipitacao"
  ) %>%
  dplyr::transmute(
    code_muni = stringr::str_sub(code, 1, 6),
    mes = as.numeric(stringr::str_extract(mes, "\\d+")) + 1,
    precipitacao
  ) %>%
  dplyr::arrange(code_muni, mes)

# Base final --------------------------------------------------------------

da_final <- da_mes %>%
  dplyr::left_join(pnud, "code_muni") %>%
  dplyr::left_join(capital, "code_muni") %>%
  dplyr::left_join(fogo, c("code_muni", "mes")) %>%
  dplyr::mutate(n_focos = ifelse(n_focos == 0, 1, n_focos)) %>%
  dplyr::left_join(casos, c("code_muni", "mes")) %>%
  dplyr::left_join(precip, c("code_muni", "mes")) %>%
  dplyr::left_join(desmatamento, c("code_muni", "mes")) %>%
  tidyr::replace_na(list(area_mun_km = 0))

# Export -----------------------------------------------------------------------

# bases auxiliares e intermediárias
readr::write_rds(da_srag_full, "data-tidy/srag_full.rds", compress = "xz")
readr::write_rds(da_srag_cat, "data-tidy/srag_tidy.rds", compress = "xz")
readr::write_rds(pnud, "data-tidy/pnud.rds")
readr::write_rds(da_dia, "data-tidy/srag_pm25_dia.rds", compress = "xz")
readr::write_rds(da_mes, "data-tidy/srag_pm25_mes.rds")
readr::write_rds(mun_amazonia, "data-tidy/code_muni_amazonia.rds")
readr::write_rds(fogo, "data-tidy/fogo.rds")
readr::write_rds(desmatamento, "data-tidy/desmatamento.rds")
readr::write_rds(casos, "data-tidy/casos.rds")
readr::write_rds(precip, "data-tidy/precipitacao.rds")

# base final
readr::write_rds(da_final, "data-tidy/base_final.rds")
readr::write_rds(da_final, "app/data/base_final.rds")

