import::from(magrittr, "%>%")

da_model <- readr::read_rds("data-tidy/base_final.rds")

da_kmedias <- da_model %>%
  dplyr::filter(dplyr::between(mes, 7, 10), !is.na(precipitacao)) %>%
  dplyr::group_by(code_muni) %>%
  dplyr::summarise(
    internacoes = mean(srag),
    pm25 = mean(pm25),
    dias_acima_25 = mean(dias_acima_25),
    precipitacao = mean(precipitacao),
    area_mun_km = mean(area_mun_km),
    pop = mean(pop),
    n_focos = mean(n_focos)
  )
readr::write_rds(da_kmedias, "app/data/da_kmedias.rds")

da_scaled <- da_kmedias %>%
  tibble::column_to_rownames("code_muni") %>%
  scale()

# kmeans ------------------------------------------------------------------

library(factoextra)
library(cluster)

fviz_nbclust(da_scaled, kmeans, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(da_scaled,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

set.seed(923)

#perform k-means clustering with k = 4 clusters
km <- kmeans(da_scaled, centers = 5, nstart = 25)
readr::write_rds(km, "data-tidy/km.rds")
km$centers

fviz_cluster(km, data = da_scaled)

da_clustered <- da_kmedias %>%
  dplyr::bind_cols(cluster = km$cluster)


pnud <- abjData::pnud_min %>%
  dplyr::filter(ano == "2010") %>%
  dplyr::mutate(muni_id = stringr::str_sub(muni_id, 1, 6)) %>%
  dplyr::transmute(
    code_muni = muni_id,
    muni_nm,
    uf = uf_sigla
  )
da_clustered <- da_clustered %>%
  dplyr::inner_join(pnud, "code_muni") %>%
  dplyr::relocate(muni_nm, uf, .after = code_muni)

readr::write_rds(da_clustered, "app/data/da_clustered.rds")



