
library(lubridate)
source("./code/spatial_toobox.R")

# 1. Consolidating dates ----
args(cams_consolidation)
start_date <- ymd('2019-01-01')
end_date <- ymd('2020-12-31')
cams_consolidation(start_date, end_date)

# 2. calculating daily mean ----
args(cams_dailyMean)
cams_dailyMean(rasterPath = "./Data/Raw/CAMS/NRT/consolidated/hourly")

# 3. getting daily average for each municipality
library(sf)
# Municipality
mun <- readRDS("./data-tidy/municipios.rds") %>% st_transform(4326) 
mun <- mun%>% dplyr::mutate(ID = 1:nrow(mun))
dailyMeanMun20 <- exact_extractValues(
  rasterPath = "./Data/Raw/CAMS/NRT/consolidated/daily_mean",
  pts_sf = mun,
  functions = c("mean", "median", "max"),
  append_cols =  c("ID", 
                   "code_muni", 
                   'name_muni',
                   "code_state",
                   "abbrev_state"))
dailyMeanMun20 %>% distinct(fun)
write_csv(dailyMeanMun20, './data-raw/pm25_muni_daily.csv')
