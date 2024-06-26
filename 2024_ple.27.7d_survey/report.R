## Prepare plots and tables for report

## Before:CGFS data from 1990-2021 in DATRASraw format including abundance at age matrix.
## Survey index model fit, retrospective fits and sensitivity analysis runs
## After: Plots and tables

rm(list=ls())
gc()

library(icesTAF)
library(FLCore)
# 
library(dplyr)
library(tidyr)
# library(tidyverse)
library(ggplot2)
# library(stringr)
library(lubridate)
library(rnaturalearth)
# library(xts)
# library(sp)
# library(gstat)
# library(caret)
# library(brinla)
# library(boot)
# library(ggrepel)
# library(icesAdvice)
# 
# source("utilities.R")

mkdir("report")
library(DATRAS)
library(surveyIndex)

source("utilities.R")

cbPalette <- c("#000000","#999999","#FFFFFF","#994F00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

wg_year <- 2024

### INPUT DATA ####

# LOAD dataset
load(paste0("data/cgfs_nage", wg_year,".Rdata"))
load("data/cgfs_DATRAS_ALK.Rdata")

# check difference of location between Gwen Drez and Thalassa
taf.png(paste0("CGFS_1990_", wg_year-1))
plot(cgfs, plot.response =F)
plot(subset(cgfs, as.numeric(as.character(Year)) > 2014), col="#E69F00", plot.response =F, add=T)
plot(subset(cgfs, Year == "2020"), col =  "#0072B2", plot.response =F, add = T)
plot(subset(cgfs, Year == as.character(wg_year-1)), col =  "#D55E00", plot.response =F, add = T)
legend(x=0.94, y = 49.5, c("Gwen Drez 1988-2014",paste0("Thalassa 2015-", wg_year-1),"Thalassa 2020", paste0("Thalassa ",wg_year-1)),pch=16, col=c("black","#E69F00", "#0072B2","#D55E00"),bty="n")
dev.off()

# plot raw data abundance distribution by survey
spdf_NS <- ne_countries(scale = 10, continent = c('Europe'), returnclass = 'sf')

##### 1.1. IBTS Q3 #### 
dat <- read.taf(paste0('data/CGFSQ4_nums_1990_', wg_year-1,'.csv'))

taf.png(file.path(paste0('report/CGFSQ4_nums_1990_', wg_year-1)))
ggplot(data = spdf_NS) + geom_sf() + coord_sf(xlim = c(-3,3), ylim = c(49,52), expand = FALSE) + geom_point(data=dat, aes(x = lon, y = lat, shape = 4), size = 0.01, color = 'red') + scale_shape_identity() + geom_point(data=dat, aes(x = lon, y = lat, size = nums)) + facet_wrap(~Year, ncol = 7) + theme_bw()
dev.off()

# plot ALK
for( i in 1:length(fdys)){
  taf.png(paste("report/ALK",i+2007, sep = "_"))
  plotALKfit(fdys[[i]], row = 1,col=cbPalette[-3])
  legend(x=50, y = 0.8, c(0:7),lty = c(1:5,1,2,3),lwd=1.7, col=cbPalette[-3],bty="n",title="Ages")
  dev.off()
}

### MODEL DIAGNOSTICS ####
load(file=paste0("data/cgfs_nage", wg_year,".Rdata"))
load("model/cgfs_gam6_ln_noship_hk.Rdata")
load("output/residuals.Rdata")
# internalCons(get("gam6_noship_hk")$idx, do.plot=TRUE)
# Explore residuals

taf.png("gam_residuals_age.png")
surveyIdxPlots(gam6_noship_hk, cgfs, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(4,3,1,1)),
               select=c("residuals"), plotByAge=F)
dev.off()

taf.png("gam_residuals_year.png")
surveyIdxPlots(gam6_noship_hk, cgfs, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(4,3,1,1)),
               select=c("resVsYear"), plotByAge=FALSE)
dev.off()

for(y in unique(gam6_noship_hk$yearNum)){
  taf.png(paste("gam_residuals_map",y,".png", sep = "_"))
  surveyIdxPlots(gam6_noship_hk, cgfs, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(4,3,1,1)),
                 select=c("spatialResiduals"), plotByAge=FALSE, year=y, map.cex=0.5)
  dev.off()
  print(y)
}

taf.png("gam_depth.png")
surveyIdxPlots(gam6_noship_hk, cgfs, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(4,3,1,1)),
               select=c("2"), plotByAge=FALSE)
dev.off()

for(a in unique(spcorr$age)){
  taf.png(paste("gam_spcorr_age", a,".png"))
  ggplot(spcorr, aes(x = dist, y = gamma , fill = as.factor(age), colour = as.factor(age))) + geom_point() + geom_smooth(method = "gam") + facet_wrap(.~ year) + ylim(0,2)
  dev.off()
  print(a)
}

# Normality of residuals go for delta-LN
taf.png("gam_qqplot.png")
par(list(mfrow = c(3,3)))
for(a in unique(resid_CGFS$age)){
  res <- resid_CGFS$res[resid_CGFS$age==a]
  qqnorm(res, main = paste("QQ plot : Age ",a))
  qqline(res)
}
dev.off()

# check temporal autocorrelation
taf.png("gam_acf.png")
par(list(mfrow = c(3,3)))
for(a in unique(resid_CGFS$age)){
  res <- resid_CGFS[resid_CGFS$age==a,c("Year", "res")]
  res$year <- ymd(res$Year, truncated = 2L)
  res <- ts(res)
  acf(res[,2], main = paste("Age ",a),max.mfrow = 1, ylim= c(-0.1,0.1))
}
dev.off()

# explore retro
retro.idx <- read.taf(file = paste0("output/GAM_CGFS_indices1990-", wg_year-1,"_retro.csv"))
retro.summary <- read.taf(file = "output/GAM_retro.csv")

# plot retro
cbPalette <- c(NA, NA, NA, NA, NA, "#999999")
cbbPalette <- c("#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00","#000000")

idx.std <- F
retro.idx$retro <- as.character(retro.idx$retro)

taf.png("gam_retro.png")
if(idx.std){
  ggplot(data=retro.idx, aes(x = Year, y = index, ymin = std.lo, ymax = std.up, fill = retro, color = retro)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
} else {
  ggplot(data=retro.idx, aes(x = Year, y = index, ymin = lo, ymax = up, fill = retro, color = retro)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
}
dev.off()

taf.png("gam_retro_CV.png")
ggplot(data=retro.idx, aes(x = Year, y = CV, fill = retro, color = retro)) + geom_line() + facet_wrap(~age, ncol = 3) + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
dev.off()

indices <- si2FLIndex(gam6_noship_hk)
taf.png(paste("gam_internal_consistency",y,".png", sep = "_"))
plot(indices,type="internal")
dev.off()

# # plot sensi test UK EEZ
# sensi.idx <- read.taf("output/GAM_CGFS_indices1990-2022_UKsensi.csv")
# cbPalette <- c(rep(NA, length(unique(sensi.idx$year_wouk))-1), "#999999")
# cbbPalette <- c(rep("#009E73", length(unique(sensi.idx$year_wouk))-1), "#000000")
# 
# taf.png("gam_UK_EEZ_sensi.png")
# if(idx.std) ggplot(data=sensi.idx , aes(x = Year, y = std.idx, ymin = std.lo, ymax = std.up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# ggplot(data=sensi.idx , aes(x = Year, y = index, ymin = lo, ymax = up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# dev.off()
# 
# taf.png("gam_UK_EEZ_sensi_CV.png")
# ggplot(data=sensi.idx , aes(x = Year, y = CV, fill = year_wouk, color = year_wouk)) + geom_line() + facet_wrap(~age, ncol = 3) + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# dev.off()
# 
# # plot sensi test UK 6miles
# sensi.idx <- read.taf("output/GAM_CGFS_indices1990-2022_UK6miles_sensi.csv")
# cbPalette <- c(rep(NA, length(unique(sensi.idx$year_wouk))-1), "#999999")
# cbbPalette <- c(rep("#009E73", length(unique(sensi.idx$year_wouk))-1), "#000000")
# 
# taf.png("gam_UK6miles_sensi.png")
# if(idx.std) ggplot(data=sensi.idx , aes(x = Year, y = std.idx, ymin = std.lo, ymax = std.up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# ggplot(data=sensi.idx , aes(x = Year, y = index, ymin = lo, ymax = up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# dev.off()
# 
# taf.png("gam_UK6miles_sensi_CV.png")
# ggplot(data=sensi.idx , aes(x = Year, y = CV, fill = year_wouk, color = year_wouk)) + geom_line() + facet_wrap(~age, ncol = 3) + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# dev.off()
# 
# # plot sensi test UK 6miles only in 2021
# sensi.idx <- read.taf("output/GAM_CGFS_indices1990-2021_UK6miles_sensi2021.csv")
# cbPalette <- c(rep(NA, length(unique(sensi.idx$year_wouk))-1), "#999999")
# cbbPalette <- c(rep("#009E73", length(unique(sensi.idx$year_wouk))-1), "#000000")
# 
# taf.png("gam_UK6miles_sensi2021.png")
# if(idx.std) ggplot(data=sensi.idx , aes(x = Year, y = std.idx, ymin = std.lo, ymax = std.up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# ggplot(data=sensi.idx , aes(x = Year, y = index, ymin = lo, ymax = up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# dev.off()
# 
# taf.png("gam_UK6miles_sensi2021_CV.png")
# ggplot(data=sensi.idx , aes(x = Year, y = CV, fill = year_wouk, color = year_wouk)) + geom_line() + facet_wrap(~age, ncol = 3) + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# dev.off()
# 
# # plot sensi test UK 6miles from 2017-2021
# sensi.idx <- read.taf("output/GAM_CGFS_indices1990-2021_UK6miles_sensi2017_2021.csv")
# cbPalette <- c(rep(NA, length(unique(sensi.idx$year_wouk))-1), "#999999")
# cbbPalette <- c(rep("#009E73", length(unique(sensi.idx$year_wouk))-1), "#000000")
# 
# taf.png("gam_UK6miles_sensi2017_2021.png")
# if(idx.std) ggplot(data=sensi.idx , aes(x = Year, y = std.idx, ymin = std.lo, ymax = std.up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# ggplot(data=sensi.idx , aes(x = Year, y = index, ymin = lo, ymax = up, fill = year_wouk, color = year_wouk)) + geom_line() + geom_ribbon(alpha = 0.2, colour = NA) + facet_wrap(~age, ncol = 3, scales = "free_y") + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# dev.off()
# 
# taf.png("gam_UK6miles_sensi2017_2021_CV.png")
# ggplot(data=sensi.idx , aes(x = Year, y = CV, fill = year_wouk, color = year_wouk)) + geom_line() + facet_wrap(~age, ncol = 3) + theme_bw() + scale_fill_manual(values = cbPalette) + scale_color_manual(values = cbbPalette)
# dev.off()
# 
# # plot abundance
# for(y in unique(gam6_noship_hk$yearNum)){
#   col <- 1:length(gam6_noship_hk$pModels)
#   taf.png(paste("gam_map",y,".png", sep = "_"))
#   if (y == "1998") col <- head(col,6)
#   surveyIdxPlots_Abmaps(gam6_noship_hk, cgfs, myids=grid[[3]], par=list(mfrow=c(3,3), mar=c(4,3,1,1)),
#                  select=c("map"), plotByAge=FALSE, cols = col, year=y, legend=FALSE)
#   dev.off()
#   print(y)
# }
