## Extract results of interest, write TAF output tables
## Before: input.RData (data), results.RData, retro.RData, sole.rep, sole.std (model)
## After:  fatage.csv, natage.csv, output.RData, res_discards.csv,
##         res_landings.csv, res_survey_fr.csv, res_survey_uk.csv,
##         summary.csv (output)

library(icesTAF)#3.0.0
library(icesAdvice)
library(reshape)
suppressMessages(taf.library(FLAssess))
suppressMessages(taf.library(FLCore))
source("utilities_model.R")

mkdir("output")

stock.name <- "ple.27.7d" 
assyear <- 2023

#load output and input data
load(paste0("data/input_", assyear, ".Rdata"))     # obs -> stock.orig
load("model/results.RData")  # fit -> stock
load("model/retro.RData")    # retro.dat

minyear <- range(stock)[["minyear"]]
maxyear <- range(stock)[["maxyear"]]

## Update stock object, save output.RData
stock.orig <- stock #make spare copy of stock object that we use later to compare estimated versus obersved landings and discards

#Generating a global object
harvest(stock)    <- results@harvest
stock.n(stock)    <- results@stock.n
stock.wt(stock) <- results@stock.wt
stock(stock)   <- apply(stock.n(stock) * stock.wt(stock), 2, sum)
discards.n(stock) <- results@discards.n

#Assumption on discards.wt, as not estimated by the model: average of the 2006:max_year values
stock@discards.wt[,ac(minyear:2005)] <-
  apply(stock@discards.wt[,ac(2006:maxyear)], 1, mean, na.rm=T)
stock@discards.wt[7, ac(c(2006,2007,2008,2012,2020))] <-
  mean(stock@discards.wt[7,], na.rm=TRUE) #missing age 7 weight@age

landings.n(stock) <- results@landings.n
landings.wt(stock)<- results@landings.wt
landings(stock)   <- apply(landings.n(stock) * landings.wt(stock),2,sum)
discards(stock)   <- apply(discards.n(stock) * discards.wt(stock),2,sum)
catch(stock)      <- discards(stock) + landings(stock)
catch.n(stock)    <- landings.n(stock) + discards.n(stock)
catch.wt(stock)   <- (landings.wt(stock)*landings.n(stock) +
                     discards.wt(stock)*discards.n(stock)) /
                     (landings.n(stock)+discards.n(stock))

ass.stock <- stock #to not confuse with stock in data.R
save(control, indices, indices.assess, results, ass.stock, stock.orig, file="output/output.RData")

## save stock object separately for mixfish
save(ass.stock, file=paste0("output/", assyear, "stockobject_mixfish_", stock.name,".RData"))

## Calculate MohnRho indices (n=5)
retro.MohnRho <- retrospective_MohnRho(retro.dat)
MohnRho <- c(Fbar = mohn(retro.MohnRho$MeanF), Recruits = mohn(retro.MohnRho$Recruits), SSB = mohn(retro.MohnRho$SSB))

save(retro.dat, retro.MohnRho, MohnRho, file="output/retro_mohnrho.RData")

## Residuals
res_landings <- flr2taf(log1p(landings.n(stock.orig)) - log(results@landings.n))
res_discards <- flr2taf(log1p(discards.n(stock.orig)) - log(results@discards.n))
res_survey_uk <- flr2taf(trim(results@index.res[[1]],
                              age=1:6, year=1989:maxyear))
res_survey_fr <- flr2taf(trim(results@index.res[[2]],
                              age=1:6, year=1993:maxyear))

## Fishing mortality and numbers at age
fatage <- flr2taf(results@harvest)
natage <- flr2taf(results@stock.n)

## Summary by year
Year <- minyear:maxyear
Rec <- stock.n(stock)[1,][drop=TRUE]
SSB <- apply(stock.n(stock) * stock.wt(stock) * mat(stock), 2, sum)[drop=TRUE]
Catch <- catch(stock)[drop=TRUE]
Landings <- landings(stock)[drop=TRUE]
Discards <- discards(stock)[drop=TRUE]
Biomass <- stock(stock)[drop=TRUE]
Fbar <- apply(results@harvest[3:6], 2, mean)[drop=TRUE]
ci <- function(x) data.frame(lo=x$mean-2*x$stddev, hi=x$mean+2*x$stddev)
Rec_lo <- exp(ci(results@stdfile[results@stdfile$name=="log_initpop",]
                 [1:length(Year),]))$lo
Rec_hi <- exp(ci(results@stdfile[results@stdfile$name=="log_initpop",]
                 [1:length(Year),]))$hi
SSB_lo <- ci(results@stdfile[results@stdfile$name=="SSB",])$lo
SSB_hi <- ci(results@stdfile[results@stdfile$name=="SSB",])$hi
Fbar_lo <- ci(results@stdfile[results@stdfile$name=="Fbar",])$lo
Fbar_hi <- ci(results@stdfile[results@stdfile$name=="Fbar",])$hi
summary <- data.frame(Year, Rec, Rec_lo, Rec_hi, SSB, SSB_lo, SSB_hi, Catch,
                      Landings, Discards, Biomass, Fbar, Fbar_lo, Fbar_hi)

## Rename plus group
res_landings <- plus(res_landings)
res_discards <- plus(res_discards)
fatage <- plus(fatage)
natage <- plus(natage)

## SAG without 2019
landings <- landings(stock.orig)
discards <- discards(stock.orig)
Catches <- landings + discards
SAG_table <- data.frame(Year, Rec_lo, as.numeric(Rec), Rec_hi, SSB_lo, as.numeric(SSB), SSB_hi, 
                        as.numeric(Catches), as.numeric(landings), as.numeric(discards), Fbar_lo,
                        as.numeric(Fbar), Fbar_hi)
names(SAG_table) <- c("Year", "Rec_lo", "Rec", "Rec_hi", "SSB_lo", "SSB", "SSB_hi", 
                      "Catches", "Landings", "Discards", "Fbar_lo", "Fbar", "Fbar_hi")                         

## Write tables to output directory
setwd("output")
write.taf(res_landings)   # 3.1.1
write.taf(res_discards)   # 3.1.2
write.taf(res_survey_uk)  # 3.1.3a
write.taf(res_survey_fr)  # 3.1.3b
write.taf(fatage)         # 3.1.4
write.taf(natage)         # 3.1.5
write.taf(summary)        # 3.1.6
write.taf(SAG_table)
setwd("..")


# Calcule du MohnRho indice (n=5)
MohnRho_SSB <- mohn(retro.MohnRho$SSB, plot = T, lwd = 2)
MohnRho_Recruits <- mohn(retro.MohnRho$Recruits, plot = T, lwd = 2)
MohnRho_MeanF <- mohn(retro.MohnRho$MeanF, plot = T, lwd = 2)
MohnRho_index <- data.frame(SSB=MohnRho_SSB, Recruits=MohnRho_Recruits, MeanF=MohnRho_MeanF)
write.csv2(MohnRho_index, "output/MohnRho_Index2.csv", row.names = FALSE)
