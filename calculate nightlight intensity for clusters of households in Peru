#OBJECTIVE: FINDS COORDINATES FROM DHS SURVEY OF PERU AND CALCULATES NIGHTLIGHT INTENSITY USING SATELLITE IMAGERY FROM NOAA
#USES DHS 2011 OF PERU
# DHS can be found here -- > http://www.measuredhs.com/Data/

#### Preliminaries: Load packages, create new folders, define aggregation functions ####
setwd('C:/Users/Rene/Desktop/Housing studies/nightlights/data/nl') 
rm(list=ls())
install.packages(c('R.utils', 'magrittr', 'foreign', 'raster', 'readstata13', plyr), dependencies = T)
library(magrittr)
library(foreign)
library(raster)
library(readstata13) # PERU DHS WAS IN STATA 13 FORMAT !!!
extract <- raster::extract # Ensure the 'magrittr' package does not mask the 'raster' package's 'extract' function
library(plyr)
'%&%' <- function(x,y)paste0(x,y)

dir.create('data/output/DHS', showWarnings = F)

# Assign each cluster the mean nightlights values over a 10 km^2 area centered on its provided coordinates
nl <- function(df, year){
  nl <- raster(paste0('C:/Users/Juan/Desktop/nightlights/data/input/Nightlights/', year, '/', list.files(paste0('C:/Users/Rene/Desktop/Housing studies/nightlights/data/input/Nightlights/', year))))
  df2 <- subset(df, is.na(lat)==F & is.na(lon)==F & lat !=0 & lon != 0)
  df2 <- unique(df2[,c('lat', 'lon')])
  shape <- extent(c(range(c(df2$lon-0.5, df2$lon+0.5)),
                    range(c(df2$lat-0.5, df2$lat+0.5))))
  nl <- crop(nl, shape)
  for (i in 1:nrow(df2)){
    lat <- sort(c(df2$lat[i] - (180/pi)*(5000/6378137), df2$lat[i] + (180/pi)*(5000/6378137)))
    lon <- sort(c(df2$lon[i] - (180/pi)*(5000/6378137)/cos(df2$lat[i]), df2$lon[i] + (180/pi)*(5000/6378137)/cos(df2$lat[i])))
    ext <- extent(lon, lat)
    nl.vals <- unlist(extract(nl, ext))
    nl.vals[nl.vals==255] <- NULL
    df2$nl[i] <- mean(nl.vals, na.rm = T)
  }
  df <- merge(na.omit(df2), df, by = c('lat', 'lon'))
  return(df)
}

# Aggregate household-level data to cluster level
cluster <- function(df, dhs = F){
  # Record how many households comprise each cluster
  for (i in 1:nrow(df)){
    sub <- subset(df, lat == df$lat[i] & lon == df$lon[i])
    df$n[i] <- nrow(sub)
  }
  # Clustering for DHS survey data
  df<-ddply(df, .(lat, lon), summarise,
            nl = mean(nl),
            n = mean(n))
  return(df)
}

#### Write DHS Data ####
path <- function(iso){
  return(paste0('C:/Users/Rene/Desktop/nightlights/data/input/DHS/',list.files('C:/Users/Rene/Desktop/nightlights/data/input/DHS/')[substr(list.files('C:/Users/Rene/Desktop/Housing studies/nightlights/data/input/DHS/'),1,2)==iso], '/'))
}

vars <- c('cluster', 'ubigeo')
names <- c('cluster', 'ubigeo')

# Peru 2011
peru11.dhs <- read.dta('C:/Users/Rene/Desktop/nightlights/data/input/DHS/perudata.dta', convert.factors=NA) %>%
  subset(select = vars)
names(peru11.dhs) <- names

peru11.coords <- read.dta('C:/Users/Rene/Desktop/nightlights/data/input/DHS/peru_coords.dta')[,c('ubigeo','cluster', 'lat', 'lon')]
names(peru11.coords) <- c('ubigeo','cluster', 'lat', 'lon')

data<-peru11.coords

for (year in 2001:2013){
  data <- peru11.coords %>%
    nl(year)
  data$year<-year
  write.csv(data, paste("nl[",year,"].csv"))
}


# OUTPUT IS IN THE FILE txt format
write.table(peru11.dhs, 'data/output/DHS/Peru 2011 DHS.txt', row.names = F) 

