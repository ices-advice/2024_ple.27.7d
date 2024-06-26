## Extract results of interest, write TAF output tables

## Before: CGFS data from 1990-2022 in DATRASraw format including abundance at age matrix.
## Survey index model fit, retrospective fits and sensitivity analysis runs
## After: Tables with model fit, index estimates, retrospectives and lack of UK station analysis at age, 
rm(list=ls())
gc()

library(icesTAF)

mkdir("output")

wg_year <- 2024

library(surveyIndex)
library(dplyr)
library(tidyr)
library(sp)
library(gstat)

source("utilities.R")

# LOAD dataset
load(paste0("data/cgfs_nage", wg_year,".Rdata"))

###############################
# deltaGAM Berg et al. 
###############################

ages <- seq(0,6)

# LOAD model outputs
load(file="model/cgfs_gam6_ln_noship_hk.Rdata")
load(file="model/cgfs_gam6_ln_noship_hk_woUK.Rdata")
load(file="model/cgfs_gam6_ln_noship_hk_woUK6miles.Rdata")
load(file="model/cgfs_gam6_ln_noship_hk_woUK6miles2022.Rdata")
load(file="model/cgfs_gam6_ln_noship_hk_woUK6miles2022_5y.Rdata")
# extract model diagnosis
model_list <- grep("retro|sensi",grep("gam",ls(), value = T), value = T, invert = T)

model_summary <- lapply(model_list, function(x){m <-get(x)
dat <- data.frame(model = x, 
                  formula = as.character(m$pModels[[1]]$formula)[3],
                  family = as.character(m$pModels[[1]]$family$family),
                  nage = length(m$pModels),
                  AIC = AIC.surveyIdx(m), 
                  BIC = AIC.surveyIdx(m, BIC = T),
                  edfs = m$edfs)
return(dat)})
model_summary <- do.call(rbind, model_summary)

write.csv(model_summary, file = "output/GAM_summary.csv",row.names = F)

# extract residuals information
d <- gam6_noship_hk
resid_CGFS <- data.frame()
for(a in ages){
  tmp <- cgfs[['HH']][,c('Year', 'lon', 'lat', 'Depth')]
  tmp$res <- d$residuals[[which(d$dataAges==a)]]
  tmp$age <- a
  resid_CGFS <- rbind(resid_CGFS,tmp)
}

res <- SpatialPointsDataFrame(resid_CGFS[,c("lon","lat")], resid_CGFS[, !names(resid_CGFS) %in% c("lon","lat")])

#attribute the Geographic projection (if in meters) or the Geographic coordinate system (if in Lat/Lon) of the data
proj4string(res) <- CRS("+init=epsg:4326") #Lat/Lon WGS84
res <- as.data.frame(spTransform(res, CRS("+init=epsg:3035")))
res <- res %>% rename(lon = coords.x1, lat = coords.x2)
res <- SpatialPointsDataFrame(res[,c("lon","lat")]/1000, res[, !names(res) %in% c("lon","lat")])

spcorr <- data.frame()

for(a in unique(res$age)){
  for(y in unique(res$Year)){
    V1 <- variogram(res ~ lon + lat , 
                    data = res[res$age == a & res$Year == y,], 
                    cressie = TRUE, cutoff=70)
    V1$age <- a
    V1$year <- y
    spcorr <- rbind(spcorr, V1)
  }
}

save(resid_CGFS, spcorr, file = "output/residuals.Rdata")

# extract index at age
idx_1 <- gather.idx(gam6_noship_hk)

write.taf(idx_1, file = paste0("output/GAM_CGFS_indices1990-", wg_year-1,".csv"),row.names = F)

# explore MohnRho 
retro_list <- grep("retro",grep("gam",ls(), value = T), value = T)

retro_summary <- lapply(retro_list, function(x){m <-get(x)
base_m <- get(gsub("retro_","",x)) 
dat <- data.frame(model = gsub("retro_","",x), 
                  formula = as.character(base_m$pModels[[1]]$formula)[3],
                  family = as.character(base_m$pModels[[1]]$family$family),
                  nage = length(base_m$pModels),
                  mRho_age0 = ifelse(length(base_m$pModels)==6,NA,mohn.surveyIdx(m, base_m)[1]),
                  mRho_age1 = ifelse(length(base_m$pModels)==6,mohn.surveyIdx(m, base_m)[1],mohn.surveyIdx(m, base_m)[2]),
                  mRho_age2 = ifelse(length(base_m$pModels)==6,mohn.surveyIdx(m, base_m)[2],mohn.surveyIdx(m, base_m)[3]),
                  mRho_age3 = ifelse(length(base_m$pModels)==6,mohn.surveyIdx(m, base_m)[3],mohn.surveyIdx(m, base_m)[4]),
                  mRho_age4 = ifelse(length(base_m$pModels)==6,mohn.surveyIdx(m, base_m)[4],mohn.surveyIdx(m, base_m)[5]),
                  mRho_age5 = ifelse(length(base_m$pModels)==6,mohn.surveyIdx(m, base_m)[5],mohn.surveyIdx(m, base_m)[6]),
                  mRho_age6 = ifelse(length(base_m$pModels)==6,mohn.surveyIdx(m, base_m)[6],mohn.surveyIdx(m, base_m)[7]),
                  mRho_age7p = ifelse(length(base_m$pModels)==8,mohn.surveyIdx(m, base_m)[8],NA))
return(dat)})
retro_summary <- do.call(rbind, retro_summary)
write.table(retro_summary, file = "output/GAM_retro.csv" ,dec = ".", sep = ",",row.names = F)

#retrospective delta-LN 
#REDO it standardized by their one mean not the baseline one

#standirdized or not
idx.std <- F

retro.idx <- gather.retro(retro_gam6_noship_hk, bound = T)
if(idx.std){
  idx_mage <- group_by(idx_1, age) %>% summarise(idx_mean = mean(index))
  idx_1 <- group_by(idx_1, age) %>% summarise(Year=Year, std.idx = index/mean(index), std.up= up/mean(index), std.lo= lo/mean(index))
  retro.idx <- left_join(retro.idx, idx_mage)
  retro.idx <- mutate(retro.idx, std.idx = index/idx_mean,.keep = "unused")
}
idx_1$retro <- as.character(wg_year-1)
if(idx.std){
  retro.idx$std.lo <- retro.idx$std.up <- NA
} else {
  retro.idx$lo <- retro.idx$up <- NA
}

retro.idx <- retro.idx[,names(idx_1)]
retro.idx  <- rbind(idx_1, retro.idx)
retro.idx$Year <- as.numeric(retro.idx$Year)

write.taf(retro.idx, file = paste0("output/GAM_CGFS_indices1990-", wg_year-1,"_retro.csv"),row.names = F)

# #sensitivity removing UK area one year at a time
# # if(idx.std) idx_1 <- group_by(idx_1, age) %>% summarise(Year=Year, std.idx = index/mean(index), std.up= up/mean(index), std.lo= lo/mean(index))
# idx_1$year_wouk <- "baseline"
# 
# sensi.idx <- gather.sensi(gam_sensi, bound = T)
# if(idx.std){
#   sensi.idx <- group_by(sensi.idx, age, year_wouk) %>% summarise(Year=Year, std.idx = index/mean(index))
#   sensi.idx$std.up <- sensi.idx$std.lo <- NA
# } else {
#   sensi.idx$up <- sensi.idx$lo <- NA
# }
# 
# idx_1 <- idx_1[,names(sensi.idx)]
# 
# sensi.idx <- rbind(sensi.idx,idx_1)
# sensi.idx$Year <- as.numeric(sensi.idx$Year)
# sensi.idx <- subset(sensi.idx, age!=0)
# 
# write.taf(sensi.idx, file = "output/GAM_CGFS_indices1990-2022_UKsensi.csv",row.names = F)
# 
# SSE <- subset(sensi.idx, !year_wouk%in% c("2020","baseline"))
# baseline <- idx_1[,c("Year", "age","index")]
# names(baseline) <- c("Year", "age","baseline")
# baseline$Year <- as.numeric(baseline$Year)
# SSE <- left_join(SSE, baseline)
# SSE_age <- group_by(SSE, age) %>% summarise(SSE = sum(((index-baseline))^2)/sd(baseline))
# SSE_tot <- sum((SSE$index-SSE$baseline)^2)
# SSE_age
# write.taf(SSE_age, file = "output/GAM_SSE_UKtest.csv",row.names = F)
# 
# # missing uk 6 nautical miles
# 
# sensi.idx <- gather.sensi(gam_sensi_6n, bound = T)
# if(idx.std){
#   sensi.idx <- group_by(sensi.idx, age, year_wouk) %>% summarise(Year=Year, std.idx = index/mean(index))
#   sensi.idx$std.up <- sensi.idx$std.lo <- NA
# } else {
#   sensi.idx$up <- sensi.idx$lo <- NA
# }
# 
# idx_1 <- idx_1[,names(sensi.idx)]
# 
# sensi.idx <- rbind(sensi.idx,idx_1)
# sensi.idx$Year <- as.numeric(sensi.idx$Year)
# sensi.idx <- subset(sensi.idx, age!=0)
# 
# write.taf(sensi.idx, file = "output/GAM_CGFS_indices1990-2022_UK6miles_sensi.csv",row.names = F)
# 
# SSE <- subset(sensi.idx, !year_wouk%in% c("2022","baseline"))
# baseline <- idx_1[,c("Year", "age","index")]
# names(baseline) <- c("Year", "age","baseline")
# baseline$Year <- as.numeric(baseline$Year)
# SSE <- left_join(SSE, baseline)
# SSE_age <- group_by(SSE, age) %>% summarise(SSE = sum(((index-baseline))^2)/sd(baseline))
# SSE_tot <- sum((SSE$index-SSE$baseline)^2)
# SSE_age
# write.taf(SSE_age, file = "output/GAM_SSE_UK6miles_test.csv",row.names = F)
# 

# # missing uk 6 nautical miles missing in 2022
# 
# test.idx <- gather.idx(gam_sensi_6n2022)
# test.idx$year_wouk <- 2021
# 
# retro.idx <- gather.retro(retro_gam6_noship_hk, bound = T) # look for run without 2022 data to compare
# retro.idx <- subset(retro.idx, retro==2021) 
# retro.idx$year_wouk <- "baseline"
# retro.idx <- retro.idx[,names(test.idx)]
# 
# test.idx <- rbind(test.idx, retro.idx)
# test.idx$Year <- as.numeric(test.idx$Year)
# test.idx <- subset(test.idx, age!=0)
# 
# write.taf(test.idx, file = "output/GAM_CGFS_indices1990-2021_UK6miles_sensi2021.csv",row.names = F)
# 
# # missing uk 6 nautical miles missing in 2022
# 
# test.idx <- gather.idx(gam_sensi_6n2022_5y)
# test.idx$year_wouk <- "2017-2021"
# 
# retro.idx <- gather.retro(retro_gam6_noship_hk, bound = T) # look for run without 2022 data to compare
# retro.idx <- subset(retro.idx, retro==2021) 
# retro.idx$year_wouk <- "baseline"
# retro.idx <- retro.idx[,names(test.idx)]
# 
# test.idx <- rbind(test.idx, retro.idx)
# test.idx$Year <- as.numeric(test.idx$Year)
# test.idx <- subset(test.idx, age!=0)
# 
# write.taf(test.idx, file = "output/GAM_CGFS_indices1990-2021_UK6miles_sensi2017_2021.csv",row.names = F)
