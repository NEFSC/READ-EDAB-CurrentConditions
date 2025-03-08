require(raster)
require(ncdf4)
library(stats)
library(geosphere)
library(dplyr)
library(here)

#load survey data & species codes
survey <- readRDS(here::here("fish dist/survdat_2024_10.rds"))
sps <- read.csv(here::here("data-raw/sps_spring_fall_combined.csv"))
numsps=nrow(sps)

# select the 48 best model distribution speccies

spptokeep <- c(13,15,22,23,24,25,26,27,28,32,33,34,35,72,73,74,75,76,77,78,84,102,103,104,105,106,107,108,109,121,131,141,143,155,156,163,164,171,172,176,181,192,193,197,301,401,502,503)

# read in depth grid
gdepth <- raster::raster("data-raw/nes_bath_data.nc",band=1)

# read in coordinates for along shelf diagnal  diag.csv
diag <- read.csv(here::here("data-raw/diag.csv"))

# read in coordinate for coast     nes_coastline.csv
nescoast <- read.csv(here::here("data-raw/nes_coastline.csv"))

# read in coordinate for coast     nes_coast_2.csv
nescoast2 <- read.csv(here::here("data-raw/nes_coast_2.csv"))

#what's the difference?

nesc2 <- read.csv(here::here("data-raw/hersey_high_2.csv"))
# constants
radt=pi/180
R <- 6371 # Earth mean radius [km]

# load stratareas
#load("stratareas.rdata")
load(here::here("data-raw/stratareas.rdata"))
survdat <- survey$survdat

#trim the data, filter for chosen season
survdat_spring <- survdat %>%
  dplyr::select("CRUISE6","STATION","STRATUM","SVSPP","YEAR","SEASON","LAT","LON","DEPTH","ABUNDANCE","BIOMASS") %>%
  dplyr::filter(SEASON == "SPRING")

# strata to use
# offshore strata to use
CoreOffshoreStrata <- c(seq(1010,1300,10),1340, seq(1360,1400,10),seq(1610,1760,10))
# inshore strata to use, still sampled by Bigelow
CoreInshore73to12 <- c(3020, 3050, 3080 ,3110 ,3140 ,3170, 3200, 3230, 3260, 3290, 3320, 3350 ,3380, 3410 ,3440)
# combine
strata_used <- c(CoreOffshoreStrata,CoreInshore73to12)

#summarize biomass
survdat_spring <- survdat_spring %>%
  dplyr::filter(STRATUM %in% strata_used) %>% #chosen strata only
  dplyr::filter(SVSPP %in% spptokeep) %>% #chosen species only
  dplyr::distinct() %>% #gets rid of length and weight info, retains unique station info
  dplyr::mutate(LOGBIO = round(log10(BIOMASS * 10+10))) #rounded biomass scalar

#summarize stations
survdat_stations <- survdat_spring %>%
  dplyr::select("CRUISE6","STATION","STRATUM","YEAR") %>%
  dplyr::distinct()

#count how many times each stratum was sampled in each year
strata_tally <- survdat_stations %>%
  dplyr::group_by(STRATUM,YEAR) %>%
  dplyr::tally()

#filter area values for chosen strata only
stratareas <- stratareas %>%
  dplyr::filter(STRATA %in% strata_used) %>%
  dplyr::rename(STRATUM = STRATA)

#merge tally with area to be able to divide
strata <- left_join(strata_tally,stratareas,by="STRATUM") %>%
  dplyr::mutate(eff_area = AREA/n)

#join survey data with effective area
survdat_spring <- dplyr::left_join(survdat_spring,strata, by=c("YEAR","STRATUM")) 

survdat_spring <- survdat_spring %>%
  dplyr::mutate(PLOTWT = eff_area*LOGBIO) #really not sure why

# Plot stations
plot(survdat$LON[survdat_spring$YEAR==1974],survdat$LAT[survdat_spring$YEAR==1974])

# number of records to evaluate
numrecs=nrow(survdat_spring)

#======================================================================================
print("distance to coast using geosphere")

####  Geosphere package to calc distance to coastline from pts (lon,lat), returns meters
dd = array(data = NA, dim = nrow(survdat_spring))
pts = data.frame(survdat_spring$LON, survdat_spring$LAT)
line_nescoast2 = t(rbind(nescoast2$LON, nescoast2$LAT))

dd <- as.data.frame(dist2Line(pts[,], line_nescoast2))
survdat_spring$GDTOC <- dd$distance/1000 # convert meters to KM

#======================================================================================

print("diag distance using geosphere")
# Find distance to diagonal line (diag), use coordinates of nearest point to find distance to NC outerbanks (min(diag))
dd2 = array(data = NA, dim = nrow(survdat_spring))
dd2 = as.data.frame(dist2Line(pts[,], diag, distfun=distHaversine))
#Distance of closest point to data along diag line to NC coast
p1 = diag[1,] #start of line
p3 = diag[150,] #end of line
p2 = data.frame(dd2[,2], dd2[,3])
distNC = distCosine(p1, p2, r=6378137) /1000 # convert to KM (Great circle distance)
survdat_spring$GASDIST = distNC


#======================================================================================

# find cases with missing depth data
missingdepth=which(is.na(survdat_spring$DEPTH))

# fill only those records in misdepth with depth from grid
for(k in missingdepth){
  survdat_spring$DEPTH[k] = raster::extract(gdepth,cbind(survdat_spring$LON[k],survdat_spring$LAT[k])) * -1
}

#=========================================================================================

spring_summary <- survdat_spring %>%
  dplyr::group_by(SVSPP,YEAR) %>%
  dplyr::mutate(swDEPTH = sum(DEPTH * PLOTWT)/sum(PLOTWT)) %>%
  dplyr::mutate(swLAT = sum(LAT * PLOTWT)/sum(PLOTWT)) %>%
  dplyr::mutate(swLON = sum(LON * PLOTWT)/sum(PLOTWT)) %>%
  dplyr::mutate(swGASDIST = sum(GASDIST * PLOTWT)/sum(PLOTWT)) %>%
  dplyr::mutate(swGDTOC = sum(GDTOC * PLOTWT)/sum(PLOTWT)) %>%
  dplyr::select(YEAR,SVSPP,swDEPTH,swLAT,swLON,swGASDIST,swGDTOC) %>%
  dplyr::distinct()




