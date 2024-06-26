## CGFS survey index for plaice 7d data compilation. 
## Extract DATRAS CGFS data for plaice in area 7d. 
## Estimates Age-Length key per year based on available sample in DATRAS from 2008-onward, prior to 2008 used historical ALK from Ifremer.
## Build abundance at age matrix from ALK and survey length distribution per survey station and year
## Allocate survey station to EEZ for sensitivity analysis.

## Before: CGFS data DATRAS exchange format, CGFS Plaice ALK from 1990-2007 from Ifremer, and GIS map of world EEZ.
## After: CGFS ALK from 2008-onward and CGFS data from 1990-2022 in DATRASraw format including abundance at age matrix.

library(icesTAF)
library(icesDatras)
library(devtools)

# NB: if run for the first time first prepare data folder and packages using:
taf.bootstrap(software=F) # don't work properly with package do to issues with compiler, so turn off software download

# remotes::install_github("DTUAqua/DATRAS/DATRAS@26bd3f3")

# load R packages from project local library (setup by icesTAF::taf.bootstrap function)
library(DATRAS)
library(sp)
# library(rgdal)
library(data.table)
library(doParallel)
# library(rgeos)
library(sf)
library(dplyr)
library(rnaturalearth)

mkdir("data")

wg_year <- 2024
  
#extract CGFS data from DATRAS 
# or Exchange files dowlaoded from DATRAS: http://datras.ices.dk/Data_products/Download/Download_Data_public.aspx 
cgfs <- readExchange("boot/data/Exchange Data_2023-03-21 10_33_45.zip")

# from datras directly: 

cgfs_new <- readExchange("boot/data/Unaggregated trawl and biological information_2024-04-02 10_50_45.zip")

# combine dataset from last year with 2023 data
cgfs[['HH']] <- rbind(cgfs[['HH']],cgfs_new[['HH']])
cgfs[['HL']] <- rbind(cgfs[['HL']],cgfs_new[['HL']])
cgfs[['CA']] <- rbind(cgfs[['CA']],cgfs_new[['CA']])

# retain valid haul
cgfs <- subset(cgfs, HaulVal == "V")

#clean up CGFS data, error with some haul location in 2012
cgfs_HH <- cgfs[["HH"]]

# import Channel and North sea map GIS 
NE <- ne_countries(scale = 10, continent = c('Europe'), returnclass = 'sf')
NE <- as(NE,'Spatial')
proj4string(NE) <- CRS('+init=epsg:4326') #Lat/Lon WGS84

cgfs_HH <- SpatialPointsDataFrame(cgfs_HH[,c("ShootLong","ShootLat")], cgfs_HH[, !names(cgfs_HH) %in% c("ShootLong","ShootLat")])

#attribute the Geographic projection (if in meters) or the Geographic coordinate system (if in Lat/Lon) of the data
proj4string(cgfs_HH) <- CRS("+init=epsg:4326") #Lat/Lon WGS84

# get station located on land
cgfs_HH[which(!is.na(over(cgfs_HH,NE)$ne_id)),]

# only issue with longitude sign for station sampled on the 17/10/2012
cgfs[["HH"]]$ShootLong[which(!is.na(over(cgfs_HH,NE)$ne_id))] <- -cgfs[["HH"]]$ShootLong[which(!is.na(over(cgfs_HH,NE)$ne_id))] 
cgfs[["HH"]]$HaulLong[which(!is.na(over(cgfs_HH,NE)$ne_id))] <- -cgfs[["HH"]]$HaulLong[which(!is.na(over(cgfs_HH,NE)$ne_id))] 
cgfs[["HH"]]$lon[which(!is.na(over(cgfs_HH,NE)$ne_id))] <- -cgfs[["HH"]]$lon[which(!is.na(over(cgfs_HH,NE)$ne_id))] 

save(cgfs, file=paste0("data/cgfs_", wg_year,".Rdata"), compress="xz")

# SET elements as DT
setDT(cgfs[['CA']])
setDT(cgfs[['HL']])
setDT(cgfs[['HH']])

# SUBSET species
# id <- findAphia("Pleuronectes platessa", latin=TRUE)

cgfs <- subset(cgfs, Species == "Pleuronectes platessa")

# issue in 2021 Length stored in mm in LngtCm and LngtCode mistake be '.'
cgfs[["CA"]]$LngtCode <- "."
cgfs[["CA"]]$LngtCode <- as.factor(as.character(cgfs[["CA"]]$LngtCode))
cgfs[["CA"]]$LngtCm <- cgfs[["CA"]]$LngtClas/10

# Missing WingSpread for Ship 35GD should be in average 10 m
cgfs[['HH']][Ship=="35GD","WingSpread"] <- 10

# Missing WingSpread for Ship 35HT should be in average 15 m
cgfs[['HH']][Ship=="35HT" & is.na(WingSpread),"WingSpread"] <- 15

# Missing distance without groundspeed info -> use euclidian distance
cgfs[['HH']] <- as.data.frame(cgfs[['HH']])
cgfs[['HH']] <- SpatialPointsDataFrame(cgfs[['HH']][,c("ShootLong","ShootLat")], cgfs[['HH']])

#attribute the Geographic projection (if in meters) or the Geographic coordinate system (if in Lat/Lon) of the data
proj4string(cgfs[['HH']]) <- CRS("+init=epsg:4326") #Lat/Lon WGS84

# metric projection ETRS89 LAEA
cgfs[['HH']] <- spTransform(cgfs[['HH']] ,CRS("+init=epsg:3035"))
names(cgfs[['HH']]) <- gsub("ShootLong","ShootLong_d",names(cgfs[['HH']]))
names(cgfs[['HH']]) <- gsub("ShootLat","ShootLat_d",names(cgfs[['HH']]))

cgfs[['HH']] <- as.data.frame(cgfs[['HH']])

# same with haul position
cgfs[['HH']] <- SpatialPointsDataFrame(cgfs[['HH']][,c("HaulLong","HaulLat")], cgfs[['HH']])
proj4string(cgfs[['HH']]) <- CRS("+init=epsg:4326") #Lat/Lon WGS84
cgfs[['HH']] <- spTransform(cgfs[['HH']] ,CRS("+init=epsg:3035"))
names(cgfs[['HH']]) <- gsub("HaulLong","HaulLong_d",names(cgfs[['HH']]))
names(cgfs[['HH']]) <- gsub("HaulLat","HaulLat_d",names(cgfs[['HH']]))

cgfs[['HH']] <- as.data.frame(cgfs[['HH']])
setDT(cgfs[['HH']])

# calculate missing distances
cgfs[['HH']][is.na(Distance), Distance := round(sqrt((ShootLong -HaulLong)^2 + (ShootLat -HaulLat)^2),0)]
cgfs[['HH']] <- cgfs[['HH']][,!c("ShootLong","ShootLat", "HaulLong", "HaulLat")]
names(cgfs[['HH']]) <- gsub("_d", "", names(cgfs[['HH']]))

# ADD SweptArea
cgfs[['HH']][, SweptArea := Distance * WingSpread * 10^(-6)] # wing spread used for plaice as considered as non-herding species, otherwise should use door spread

# REMOVE if HaulDur < 10 min or if above 39 min 
cgfs <- subset(cgfs, HaulDur > 9)
cgfs <- subset(cgfs, HaulDur < 40)

# REMOVE night hauls?
cgfs <- subset(cgfs, DayNight == "D")

# REMOVE data outside 7D
cgfs <- subset(cgfs, lon >= -2)
cgfs <- subset(cgfs, lat <= 51)

# REMOVE Seine Estuary (27F0) not sampled by the vessel "Thalassa" (Ship=="35HT") after 2014
cgfs <- subset(cgfs, !StatRec == "27F0")

# CREATE a numeric year variable
cgfs$ctime  <-  as.numeric(as.character(cgfs$Year))

cgfs[['CA']] <- as.data.frame(cgfs[['CA']])
cgfs[['HH']] <- as.data.frame(cgfs[['HH']])
cgfs[['HL']] <- as.data.frame(cgfs[['HL']])

# check when do we have CA information
unique(cgfs[['CA']]$Year)

# include age data not available on DATRAS from French data base

#set age+ (basically age used in assessment plus 1)

Pgrp <- 7

# build N by size class per haul in the same order as HH table (example for 1990)
cgfs_tmp <- subset(cgfs, !Year %in% as.character(unique(cgfs[['CA']]$Year)))
cgfs_tmp <- subset(cgfs_tmp, !Year %in% c("1988", "1989")) # no age data before 
cgfs_tmp <- addSpectrum(cgfs_tmp, by=1)

dys <- split(cgfs_tmp, cgfs_tmp$Year)
database_ALK <- read.table("boot/data/ExtractionCleCGFS_PLEMUR.csv", sep = ",", dec = ".", header = T)
database_ALK <- subset(database_ALK, Species == "Pleuronectes platessa")
database_ALK <- subset(database_ALK, ANNEE < 2008)
database_ALK <- split(database_ALK, database_ALK$ANNEE)

pdys_old <- mapply(function(x, y) {
  SIZE <- xtabs(Count~haul.id+sizeGroup,x[["HL"]])[x[["HH"]]$haul.id,]
  y <- as.data.table((y))
  y[,`:=`(prop = NBRE/sum(NBRE)), by = LONGUEUR]
  y <- merge(expand.grid(LONGUEUR=seq(min(y$LONGUEUR),max(y$LONGUEUR),by = 1), AGE=seq(min(y$AGE),max(y$AGE),by = 1)), y,all = T)
  y$prop[is.na(y$prop)] <- 0
  y <- xtabs(prop~LONGUEUR+AGE, y)
  y <- y[sapply(regmatches(colnames(SIZE), gregexpr("[[:digit:]]+", colnames(SIZE))), function(x) x[1]),]
  y <- cbind(y[, as.numeric(colnames(y)) < Pgrp], apply(y[, as.numeric(colnames(y)) >= Pgrp],1,sum))
  colnames(y)[ncol(y)] <- paste0(Pgrp, "+")
  y <- array(y, c(dim(y),length(x[["HH"]]$haul.id)), dimnames = list(sizeGroup=colnames(SIZE), age = colnames(y),haul.id= rownames(SIZE)))
  
  SIZE <- array(SIZE, c(dim(SIZE),dim(y)[2]), dimnames = list(haul.id= rownames(SIZE), sizeGroup=colnames(SIZE), age = colnames(y))) 
  SIZE <- aperm(SIZE, c(2,3,1))
  
  y <- apply(y * SIZE, c(3,2), sum)
  
  return(y)
}, x=dys, y=database_ALK)

# --- CONSTRUCT ALK based on CA information from 2008 onward
# test ALK after 2007
cgfs_tmp <- subset(cgfs, Year %in% as.character(unique(cgfs[['CA']]$Year)))

# remove missing age in CA
cgfs_tmp <- subset(cgfs_tmp, !is.na(Age))
# cgfs_tmp <- subset(cgfs_tmp, Age>=1)

fixAgeGroup1 <- function (x, age = 0, n = 3, fun = "mean") #based on surveyIndex package
{
  cm.breaks <- attr(x, "cm.breaks")
  f <- match.fun(fun)
  d = split(x, x$Year)
  subsLength = f(x[[1]]$LngtCm[x[[1]]$Age==age], na.rm = TRUE)
  for (y in 1:length(d)) {
    nobs = sum(d[[y]][[1]]$Age == age, na.rm = TRUE)
    if (nobs < n) {
      sel = sample(1:nrow(d[[y]][[1]]), n - nobs)
      d[[y]][[1]] = rbind(d[[y]][[1]][sel, ], d[[y]][[1]])
      d[[y]][[1]][1:(n - nobs), "Age"] = age
      d[[y]][[1]][1:(n - nobs), "LngtCm"] = subsLength
      d[[y]][[1]][1:(n - nobs), "NoAtALK"] = 1
    }
  }
  dd <- do.call("c", d)
  if (!is.null(cm.breaks)) 
    dd <- addSpectrum(dd, cm.breaks = cm.breaks)
  dd
}

ages <- seq(0, Pgrp) 

for(aa in ages){
  cgfs_tmp <- fixAgeGroup1(cgfs_tmp, age=aa, n=3, fun=mean)
} 

cgfs_tmp <- addSpectrum(cgfs_tmp, by=1)

# -- STEPS in add.ALK

registerDoParallel(detectCores()-1)

# # SUBSET for age > minage
# 
# dat <- subset(dat, Age >= 1)

# SPLIT by Year

dys <- split(cgfs_tmp, cgfs_tmp$Year)

# CALL fitALK by year (without spatial effect)

fdys <- foreach(i=dys, .errorhandling = "pass") %dopar% {
  DATRAS::fitALK(i,model = "cra~LngtCm", minAge=min(ages), maxAge=max(ages),autoChooseK=F,
                 useBIC=TRUE, varCof=FALSE, maxK=50)
}

save(fdys, file="data/cgfs_DATRAS_ALK.Rdata", compress="xz")

# # DO NOT converge using spatial smooths (not enough data per year)
# fdys_sp <- foreach(i=dys, .errorhandling = "pass") %dopar% {
#   DATRAS::fitALK(i, model = "cra~LngtCm+s(lon,lat,bs='ts')", minAge=min(ages), maxAge=max(ages),autoChooseK=TRUE,
#                  useBIC=TRUE, varCof=FALSE, maxK=50)
# } 

# PREDICT NaA by year

pdys <- foreach(i=fdys, .errorhandling = "remove") %dopar% {
  predict(i)
}

# combine old and new data
cgfs <- subset(cgfs, !Year %in% c("1988", "1989"))
ages <- seq(0, Pgrp) 

for(aa in ages){
  cgfs <- fixAgeGroup1(cgfs, age=aa, n=2, fun=mean)
} 

cgfs <- addSpectrum(cgfs, by=1)

dys <- split(cgfs, cgfs$Year)

pdys <- c(pdys_old, pdys)

ndys <- mapply(function(x, y) {
  x$Nage <- y
  return(x) 
}, x=dys, y=pdys)

cgfs <- do.call("c", ndys)

# plot abundance
cgfs[['HH']]$nums <- apply(cgfs[['HH']]$N,1,sum) 
cgfs[['HH']]$nums[cgfs[['HH']]$nums==0] <- NA

write.taf(cgfs[['HH']][,c('haul.id','lat','lon','Year','nums')], file=paste0('CGFSQ4_nums_1990_', wg_year-1,'.csv'), dir='data')

# Allocate station to EEZ
taf.unzip("boot/data/World_EEZ_v6.1_20110512.zip", exdir="boot/data")
EEZ <-  sf::st_read("boot/data/World_EEZ_v6_1_20110512.shp")
EEZ <- as(EEZ,'Spatial')
proj4string(EEZ)
EEZ <- spTransform(EEZ, CRS("+init=epsg:4326")) #Lat/Lon WGS84 the same one as below

# add Country EEZ
# Transform dataframe into Spatialdataframe 
hh <- as.data.frame(cgfs[["HH"]])
hh <- SpatialPointsDataFrame(hh[,c("lon","lat")], hh[, !names(hh) %in% c("lon","lat")])

#attribute the Geographic projection (if in meters) or the Geographic coordinate system (if in Lat/Lon) of the data
proj4string(hh) <- CRS("+init=epsg:4326") #Lat/Lon WGS84

# associate a polygon to each sampling site. 
hh$EEZ <- over(hh, EEZ)$Sovereign
cgfs[['HH']]$EEZ <- hh$EEZ

# In 2022 UK 6 nautical miles were not sampled (no authorizations) test influence on model outcome

# build a buffer
p <- data.frame(lon = c(-2,-2,2.5,2.5,-2), lat = c(49,51.5,51.5,49,49))
p <- Polygons(list(Polygon(p)),1)
p <- SpatialPolygons(list(p))
proj4string(p) <- CRS("+init=epsg:4326") 

taf.unzip("boot/data/United_Kingdom_Hydrographic_Office_Maritime_Limits_and_Boundaries.zip", exdir="boot/data")
uk <- sf::st_read("boot/data/UK_Hydrographic_Office_Maritime_Limits_and_Boundaries.shp")
uk <- as(uk,'Spatial')
proj4string(uk) <- CRS("+init=epsg:4326") 

p <- as(p,'sf')
NE <- as(NE,'sf')

res <- st_difference(p, NE)

uk_6miles <- subset(uk,Nature.of.=="6NM")
uk_6miles <- as(uk_6miles,"sf")

# split bounding box by the UK 6NM
clip <- res %>% as("sf") %>%
  st_bbox() %>%
  st_as_sfc() %>%
  lwgeom::st_split(uk_6miles) %>% 
  st_collection_extract("POLYGON") %>% 
  st_as_sf() %>% 
  # calculate x coordinate of the centroid; east will be higher
  mutate(xpos = st_coordinates(st_centroid(.))[,"X"]) %>% 
  mutate(position = ifelse(xpos == max(xpos), "south", "north")) %>% 
  filter(position == "north") %>% as("Spatial")

# uk_6miles <- st_intersection(res, clip$x) 

# associate a polygon to each sampling site. 
hh$uk_6miles <- over(hh, clip)$position
cgfs[['HH']]$uk_6miles <- hh$uk_6miles


save(cgfs, uk_6miles, file=paste0("data/cgfs_nage", wg_year,".Rdata"), compress="xz")
