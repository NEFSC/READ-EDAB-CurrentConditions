##SW version
#goal is to get EPU-wide estimates of average daily sst (from OISST)
#used kevins methods to go from global netcdf to global raster then local (NE shelf) raster

#load packages
library(terra)
library(sf)
#load raster file
load("~/Desktop/CurrentConditions/oisst/RAST_NESREG_2024.12.01.AV.TEMP.1DAY.000082150.RData")
data = terra::rast(masked.raster)
#not sure why here didn't work but ok

#load shapefiles
#EPUs <- read_sf("data-raw/shp_files/adv rep MAB GOM GBK NES SCSPoly.shp")
#index.raster <- masked.raster
#index.raster[] <- 1:ncell(index.raster) #not sure what this is for, but there are 5120 cells apparently

shp.vect = terra::vect("data-raw/shp_files/adv rep MAB GOM GBK NES SCSPoly.shp")
shp.str = as.data.frame(shp.vect)

#pull out EPU names
area.names <- as.vector(shp.str$Primary)
which.att = which(apply(shp.str,2,function(x) all(area.names %in% x)))
which.area =  match(area.names,shp.str[,which.att])
#testing using terra instead
avg_sst <- c()
for(j in 1:length(area.names)){
  area.data = terra::mask(data,shp.vect[which.area[j],], touches = T)
  area.agg = terra::global(area.data,fun = mean,na.rm = T)
  area.agg <- cbind(area.agg$mean,area.names[j])
  avg_sst <- rbind(avg_sst,area.agg)
}

#make into tidy data frame
avg_sst <- as.data.frame(avg_sst) %>%
  dplyr::rename(SST = V1, EPU = V2)
