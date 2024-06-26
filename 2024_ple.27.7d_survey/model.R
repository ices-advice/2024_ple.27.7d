## Run CGFS index calculation based on Delta-Lognormal GAM approach. 
## Produce retrospective analysis and sensitivity of the abundance index to lack of UK stations.

## Before: CGFS data from 1990-2022 in DATRASraw format including abundance at age matrix.
## After: Survey index model fit, retrospective fits and sensitivity analysis runs

rm(list=ls())
gc()

# remotes::install_github("casperwberg/surveyIndex/surveyIndex@d482809")

#GAM's model
library(icesTAF)
library(DATRAS)
library(surveyIndex)
library(doParallel)
library(RcppAlgos)

mkdir("model")

wg_year <- 2024

# allocated number of core used
if(Sys.info()['sysname'] == "Linux"){
  mc.cores <- detectCores()-1 # leave 1 core free
} else {
  mc.cores <- 1
}

# LOAD data
load(paste0("data/cgfs_nage", wg_year,".Rdata"))

##############################################################
### RUN Berg et al. GAMs
##############################################################

# CREATE spatial grid

grid <- getGrid(cgfs, nLon=30)

# --- GAM LN, time-invariant spatial effect + depth

ages <- seq(0, 6)
cutoff <- 0.1

# Increased number of spatial knots using LN family and up to age 6

# SET MAX dimension of smoother for each age group
# SET no. knots spatial splines

kvecP = rep(14 * 14 , length(ages))
kvecZ = rep(10 * 10 , length(ages))


modelsStatZ <- rep("Year+s(lon,lat,k=kvecZ[a],bs='ts') + s(Depth,bs='ts')+
                   offset(SweptArea)", length(ages))
modelsStatP <- rep("Year+s(lon,lat,k=kvecP[a],bs='ts') + s(Depth,bs='ts')+
                   offset(SweptArea)", length(ages))

# run models
gam6_noship_hk <- getSurveyIdx(cgfs, ages = ages,
                               myids = grid[[3]],
                               cutOff = cutoff,
                               fam = rep("LogNormal", length(ages)),
                               modelZ = modelsStatZ, modelP = modelsStatP,
                               kvecP = kvecP, kvecZ = kvecZ,
                               mc.cores = mc.cores)

# run retrospective analysis
retro_gam6_noship_hk <- retro.surveyIdx(gam6_noship_hk, cgfs, grid)

save(grid, gam6_noship_hk, retro_gam6_noship_hk, file="model/cgfs_gam6_ln_noship_hk.Rdata", compress="xz")

# # sensitivity to lack of UK stations: delta-LN GAM on 0-6 ages
# 
# gam_sensi <- vector("list", length(levels(cgfs$Year)))
# y_sensi <- levels(cgfs$Year)
# names(gam_sensi) <- y_sensi
# 
# for(y in 1:length(y_sensi)){
#   print(y_sensi[y])
#   cgfs_sensi <- subset(cgfs, !c(Year==y_sensi[y] & EEZ=="United Kingdom")) #remove UK observation for a given year
#   
#   gam_sensi[[y]] <- getSurveyIdx(cgfs_sensi, ages = ages,
#                                  myids = grid[[3]], #same grid as for the full model try to see if can predict observation
#                                  cutOff = cutoff,
#                                  fam = rep("LogNormal", length(ages)),
#                                  modelZ = modelsStatZ, modelP = modelsStatP,
#                                  kvecP = kvecP, kvecZ = kvecZ,
#                                  mc.cores = mc.cores)
# }
# save(grid, gam_sensi,  file="model/cgfs_gam6_ln_noship_hk_woUK.Rdata", compress="xz")
# 
# # test sensitivity to lack of UK stations within 6 nautical miles over several years (1, 2 ,3...): delta-LN GAM on 0-6 ages
# # comboSample(5, 2, n = 20) #20 samples of 2 years out of 5
# # sensitivity to lack of UK stations: delta-LN GAM on 0-6 ages
# 
# gam_sensi_6n <- vector("list", length(levels(cgfs$Year)))
# y_sensi_6n <- levels(cgfs$Year)
# names(gam_sensi_6n) <- y_sensi_6n
# 
# for(y in 1:length(y_sensi_6n)){
#   print(y_sensi_6n[y])
#   cgfs_sensi_6n <- subset(cgfs, !c(Year==y_sensi_6n[y] & !is.na(uk_6miles))) #remove within UK 6 nautical miles observation for a given year
#   
#   gam_sensi_6n[[y]] <- getSurveyIdx(cgfs_sensi_6n, ages = ages,
#                                  myids = grid[[3]], #same grid as for the full model try to see if can predict observation
#                                  cutOff = cutoff,
#                                  fam = rep("LogNormal", length(ages)),
#                                  modelZ = modelsStatZ, modelP = modelsStatP,
#                                  kvecP = kvecP, kvecZ = kvecZ,
#                                  mc.cores = mc.cores)
# }
# save(grid, gam_sensi_6n,  file="model/cgfs_gam6_ln_noship_hk_woUK6miles.Rdata", compress="xz")
# 
# #test lack of UK stations within 6 nautical miles in 2021 in assessment year 2022
# 
# cgfs_sensi_6n2022 <- subset(cgfs, !c(Year==2021 & !is.na(uk_6miles)) & Year!=2022) #remove within UK 6 nautical miles observation for a given year
# 
# gam_sensi_6n2022 <- getSurveyIdx(cgfs_sensi_6n2022 , ages = ages,
#                                   myids = grid[[3]], #same grid as for the full model try to see if can predict observation
#                                   cutOff = cutoff,
#                                   fam = rep("LogNormal", length(ages)),
#                                   modelZ = modelsStatZ, modelP = modelsStatP,
#                                   kvecP = kvecP, kvecZ = kvecZ,
#                                   mc.cores = mc.cores)
# 
# save(grid, gam_sensi_6n2022,  file="model/cgfs_gam6_ln_noship_hk_woUK6miles2022.Rdata", compress="xz")

# #test lack of UK stations within 6 nautical miles from 2021-2017 in assessment year 2022
# 
# cgfs_sensi_6n2022_5y <- subset(cgfs, !c(Year%in% c(2017:2021) & !is.na(uk_6miles)) & Year!=2022) #remove within UK 6 nautical miles observation for a given year
# 
# gam_sensi_6n2022_5y <- getSurveyIdx(cgfs_sensi_6n2022_5y , ages = ages,
#                                  myids = grid[[3]], #same grid as for the full model try to see if can predict observation
#                                  cutOff = cutoff,
#                                  fam = rep("LogNormal", length(ages)),
#                                  modelZ = modelsStatZ, modelP = modelsStatP,
#                                  kvecP = kvecP, kvecZ = kvecZ,
#                                  mc.cores = mc.cores)
# 
# save(grid, gam_sensi_6n2022_5y,  file="model/cgfs_gam6_ln_noship_hk_woUK6miles2022_5y.Rdata", compress="xz")