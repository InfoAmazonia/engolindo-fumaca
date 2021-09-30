cams_consolidation <- function(cams_path = "./Data/Raw/CAMS/NRT", start_date, end_date, by = 'day'){
  # defining function to consolidate several netCDF layers into one file with several layers, having the timezone set to GMT-5 (Acres's time zone), and values converted from Kg/m3 to mg/m3
  
  library(raster)
  library(lubridate)
  library(hms)
  
  dt <- start_date
  # defining amount of days to be analyzed
  dias <- seq(start_date, end_date, by = by)
  
  for (dia in dias){
    cat(paste(as_date(dia),'\n'))
    # listing netCDF with date pattern
    files <- list.files(path = cams_path, pattern = paste(
      as_date(dia)), 
      full.names = T)
    
    fileName_split <- stringr::str_split(files[1], "/")[[1]]
    filename <- fileName_split[length(fileName_split)]
    filePath <- gsub(pattern = filename, replacement = '', x = files[1])
    filePath <- paste0(filePath, 'consolidated/hourly/')
    filename <- paste0(
      paste(
        stringr::str_split(filename, "_")[[1]][1:2], collapse = '_'), '_hourly.nc')
    paste(filename)
    
    #creating folder for consolidates images
    if(!dir.exists(filePath)){
      dir.create(filePath)}
    # If file already exists it will be skipped
    if(file.exists(paste0(filePath, filename))){
      cat("skipped ", filename, " \n")
      next
    }
    
    # reading netCDF
    r <- stack()
    for(i in files){
      r_intermediario <- stack(i)
      r <- stack(r, r_intermediario)
    }
    
    # organizing bandname
    bandName <- gsub('X', '', names(r))
    images_ymd_hms <- ymd_hms(bandName)
    image_ymd <- date(images_ymd_hms)
    # transforming in UTC-5
    hour(images_ymd_hms) <- hour(images_ymd_hms) - 5
    # saving as bandname with pattern easy to extract later
    names(r) <- paste0('ymd_', image_ymd,'_',as_hms(images_ymd_hms))
    
    # converting from Kg m3 to mg/m3
    r <- r*1e9
    writeRaster(r, 
                paste0(filePath, filename), overwrite=TRUE, 
                format="CDF", varname="pm2p5", varunit="ug", 
                longname="ppm2.5", xname="Longitude",   
                yname="Latitude", zname="Time UTC-5")
    cat(filename, "\n")
  }
  
}

cams_dailyMean <- function(c = "./Data/Raw/CAMS/NRT/consolidated/hourly"){
  # consolidate CAMS hourly raster to daily mean
  # output set to 'daily_mean/'
  library(raster)
  
  rasterPath_split <- stringr::str_split(rasterPath[1], "/")[[1]]
  newPath <- paste0(
    rasterPath_split[1:length(rasterPath_split)-1], collapse = "/")
  newPath <- paste0(newPath, '/daily_mean/')
  
  #creating folder for consolidates images
  if(!dir.exists(newPath)){
    dir.create(newPath)}
  
  # listing raster files
  rasterFiles <- list.files(rasterPath, full.names = TRUE)
  cat( paste( 
    length(rasterFiles), "Files found\n"))
  Sys.sleep(2)
  
  for (rasterFile in rasterFiles){
    rasterFileSplit <- stringr::str_split(rasterFile, "/")[[1]]
    filename <- paste0(
      gsub('hourly', "mean", rasterFileSplit[length(rasterFileSplit)]))
    
    # if file already exists, will be skipped
    if(file.exists(paste0(newPath, filename))){
      cat(filename, "já processado \n\n")
      next}
    
    date <- stringr::str_split(rasterFileSplit[length(rasterFileSplit)], "_")[[1]][2]
    r <- stack(rasterFile)
    r.mean <- mean(r)
    writeRaster(r.mean, 
                paste0(newPath, filename), overwrite=TRUE, 
                format="CDF", varname="pm2p5", varunit="ug", 
                longname=paste0(date), xname="Longitude",   
                yname="Latitude", zname="time")
    cat(paste0(newPath, filename, " done \n\n"))  
  }
}

exact_extractValues <- function(rasterPath = "./Data/Raw/CAMS/NRT/consolidated/daily_mean", 
                                pattern = '2020',
                                pts_sf = mun,
                                functions = c("mean", "median", "max"),
                                append_cols = c("ID", "code_muni", "code_state")){
  # extract values from CAMS daily mean raster considering spatial sf class using  exactextractr::exact_extract()
  library(lubridate)
  library(magrittr)
  library(raster)
  library(sf)
  library(exactextractr)
  library(dplyr)
  library(tidyr)
  
  pts_sf <- pts_sf %>% 
    dplyr::select(append_cols)
  
  # list of raster files
  rasterFiles <- list.files(rasterPath, pattern, full.names = TRUE)
  r <- stack()
  for(rasterFile in rasterFiles){
    r <- stack(r, rasterFile)
  }
  cat("raster stack done \n")
  
  # extracting values from raster stack
  values <- exact_extract(
    r,
    pts_sf,
    fun = functions, 
    force_df = TRUE,
    append_cols = append_cols)
  
  # Converting to tibble
  values2 <- values %>% tibble::as_tibble()
  
  values2 <- values2 %>%
    dplyr::relocate(
      append_cols) %>% pivot_longer(
        -c(append_cols),
        names_to = 'date',
        values_to = 'ppm25')
  
  values2 <- values2 %>%
    separate(date, c('fun', 'date'), sep = '\\.X') %>% 
    mutate(
      date = date(
        gsub('[.]', '-', date)
      ))
  
  values2 <- values2 %>%
    group_by(code_state, code_muni, name_muni, zone, fun, date) %>% 
    summarise(ppm25 = mean(ppm25)) %>% ungroup()

    return(values2)

}