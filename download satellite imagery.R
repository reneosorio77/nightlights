#PROJECT: NIGHTLIGHTS PROXY FOR ECONOMIC ACTIVITY from 2000-2013
#OBJECTIVE: DOWNLOAD IMAGES IN COMPRESSED FORMAT TAR
#SOURCE WEBSITE: https://www.ngdc.noaa.gov/eog/data/web_data/v4composites/


setwd('C:/Users/JuanPerez/nightlights') # Set working directory to where you download your images
rm(list=ls())
library(R.utils)

# Downloads nightlights data (about 400 MB zipped and 750 MB unzipped per year)
dir.create('data/input/Nightlights')
for (year in 2010:2013){
  url <- paste0('https://ngdc.noaa.gov/eog/data/web_data/v4composites', year, '.v4.tar')
  temp <- paste0('data/input/Nightlights/', year)
  dir.create(temp)
  dest <- paste0(temp, '/F18', year, '.v4.tar')
  download.file(url, dest)
  untar(dest, exdir = temp)
  file <- list.files(temp)
  file <- file[substr(file, nchar(file)-27, nchar(file))=='stable_lights.avg_vis.tif.gz']
  tif <- paste0(temp, '/', file)
  file <- substr(file, 1, nchar(file)-3)
  gunzip(tif, paste0(temp, '/', file))
  unlink(paste0(temp, '/', list.files(temp)[list.files(temp)!=file]), recursive = T)
  print(year)
  rm(dest, file, temp, tif, url, year)
}
unload(R.utils)
