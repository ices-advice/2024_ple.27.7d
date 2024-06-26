#####################################################
## Preprocess data, write TAF data tables
## Assessment year : 2024
#####################################################

library(icesTAF)
suppressMessages(taf.library(FLAssess))
suppressMessages(taf.library(FLEDA))
suppressMessages(taf.library(FLXSA))
library(splines)
suppressMessages(library(mgcv))
library(methods)
source("utilities_data.R")
source("utilities_model.R")

mkdir("data")

stock.name <- "ple.27.7d" 
assyear <- 2023


### ------------------------------------------------------------------------------------------------------
###   Explore IC stockoverview and raw data in IC  ====
### ------------------------------------------------------------------------------------------------------

## import IC raw files
taf.unzip("bootstrap/data/Numbers_at_age_and_mean_weights_at_age.zip", files="NumbersAtAgeLength.txt", exdir="bootstrap/data")
file.rename(from = "bootstrap/data/NumbersAtAgeLength.txt", paste0("bootstrap/data/NumbersAtAgeLength_", assyear, ".txt"))
StockOverviewFile <- "bootstrap/data/StockOverview_2023.txt"
NumbersAtAgeLengthFile <- paste0("bootstrap/data/NumbersAtAgeLength_", assyear, ".txt")

## Read in IC raw data
WtData <- readStockOverview(StockOverviewFile,NumbersAtAgeLengthFile) 
# add manually the number of age classes included in the text file
ages_1 <- 10
Ndata <- readNumbersAtAgeLength(NumbersAtAgeLengthFile) 
Ndata$Area<-ifelse(Ndata$Area %in% c('IVa','IVb','IVc'),'IV',
                   ifelse(Ndata$Area %in% c('VIa','VIb'),'VI',as.character(Ndata$Area)))
WtData$Area<-ifelse(WtData$Area %in% c('IVa','IVb','IVc'),'IV',
                    ifelse(WtData$Area %in% c('VIa','VIb'),'VI',as.character(WtData$Area)))

save(WtData, Ndata, file="data/StockOverview_2023.RData") 

### ------------------------------------------------------------------------------------------------------
###   Prepare raised data and build stock object for 2021 assessment
### ------------------------------------------------------------------------------------------------------

### Extract final export files from IC 
mkdir(paste0("bootstrap/data/Q2_landings_", assyear))
mkdir(paste0("bootstrap/data/Q1_landings_", assyear))
mkdir(paste0("bootstrap/data/Q1_discards_", assyear))
mkdir(paste0("bootstrap/data/all_landings_", assyear))
mkdir(paste0("bootstrap/data/all_discards_", assyear))

taf.unzip("bootstrap/data/ple.27.7d__ 2024-4-4_17_49_58_Landings_Q2.zip", exdir=paste0("bootstrap/data/Q2_landings_", assyear))
taf.unzip("bootstrap/data/ple.27.7d__ 2024-4-4_17_43_01_Landings_Q1.zip", exdir=paste0("bootstrap/data/Q1_landings_", assyear))
taf.unzip("bootstrap/data/ple.27.7d__ 2024-4-4_17_36_00_Discards_Q1.zip", exdir=paste0("bootstrap/data/Q1_discards_", assyear))
taf.unzip("bootstrap/data/ple.27.7d__ 2024-4-4_17_29_59_Discards_All.zip", exdir=paste0("bootstrap/data/all_discards_", assyear))
taf.unzip("bootstrap/data/ple.27.7d__ 2024-4-4_17_12_32_Landings_All.zip", exdir=paste0("bootstrap/data/all_landings_", assyear))

### Read stock data for the previous years 
load("bootstrap/data/stockobject_2022.RData")

## current year data All quarters Landings
currentYearLandingsAllQ <- paste0("bootstrap/data/all_landings_", assyear, "/")
## current year data First quarter Landings
currentYearLandings1Q <- paste0("bootstrap/data/Q1_landings_", assyear, "/")
## current year data Second quarter Landings
currentYearLandings2Q <- paste0("bootstrap/data/Q2_landings_", assyear, "/")
## current year data All quarters Discards
currentYearDiscardsAllQ <- paste0("bootstrap/data/all_discards_", assyear, "/")
## current year data First quarter Discards
currentYearDiscards1Q <- paste0("bootstrap/data/Q1_discards_", assyear, "/")

## add one year to the FLStock object
stock <- window(stock,end=assyear)

## read landings caton, canum and weca values for the year and add it to the stock object
caton <- read.table(paste(currentYearLandingsAllQ,"caton.txt",sep=''),skip=7,dec='.')
caton$V1 <- as.numeric(gsub(',', '', caton$V1))
stock@landings[1,dim(stock@landings)[2]] <- caton$V1

canum <- read.table(paste(currentYearLandingsAllQ,"canum.txt",sep=''),skip=7,dec='.')
canum <- as.numeric(t(gsub(',', '', t(canum))))/1000
canum <- append(canum, 0, after = 0) # only run to insert a zero for the first year (GH)
stock@landings.n[,dim(stock@landings.n)[2]] <- canum

weca <- read.table(paste(currentYearLandingsAllQ,"weca.txt",sep=''),skip=7,dec='.')	/1000
weca <- append(weca, NA, after = 0)  # only run to insert a NA for the first year (GH)
stock@landings.wt[,dim(stock@landings.wt)[2]] <- as.numeric(weca)

## read Q2 weca to set west (stock.wt)
wecaQ2 <- read.table(paste(currentYearLandings2Q,"weca.txt",sep=''),skip=7,dec='.')  /1000
stock@stock.wt[2:11,dim(stock@stock.wt)[2]] <- as.numeric(wecaQ2) # the age 1 was missing (no info) so I changed the years from 2:11 to 3:11

## read discards caton, canum and weca values for the year and add it to the stock object
caton_D <- read.table(paste(currentYearDiscardsAllQ,"caton.txt",sep=''),skip=7,dec='.')
caton_D$V1 <- as.numeric(gsub(',', '', caton_D$V1))
stock@discards[1,dim(stock@discards)[2]] <- caton_D$V1

canum_D <- read.table(paste(currentYearDiscardsAllQ,"canum.txt",sep=''),skip=7,dec='.')
canum_D <- as.numeric(t(gsub(',', '', t(canum_D))))/1000
stock@discards.n[,dim(stock@discards.n)[2]] <- 0
stock@discards.n[1:11,dim(stock@discards.n)[2]] <- canum_D

weca_D <- read.table(paste(currentYearDiscardsAllQ,"weca.txt",sep=''),skip=7,dec='.')	/1000
stock@discards.wt[1:11,dim(stock@discards.wt)[2]] <- as.numeric(weca_D)
# zero weights should be replaced by NA
# stock@discards.wt[10,dim(stock@discards.wt)[2]] <- NA # to replace the 0 weight of age 9 by NA

### Complete the other slots of the stock objects
stock@m[,dim(stock@m)[2]] <- stock@m[,1]
stock@mat[,dim(stock@mat)[2]] <- stock@mat[,1]
stock@harvest.spwn[,dim(stock@harvest.spwn)[2]] <- stock@harvest.spwn[,1]
stock@m.spwn[,dim(stock@m.spwn)[2]] <- stock@m.spwn[,1]

### Removing Q1 plaice
## option to remove a percentage of Q1 landings and number-at-age matrix (catch.n == landings.n if no discard data) from the VIId catch
## since these are fish that belong to VIIe (~15%) or area IV (~50%)
PERCENT_7d_Q1 <- 0.65

## read caton, canum and weca values for the Q1
## Landings
catonQ1 <- read.table(paste(currentYearLandings1Q,"caton.txt",sep=''),skip=7,dec='.')
catonQ1$V1 <- as.numeric(gsub(',', '', catonQ1$V1))

canumQ1 <- read.table(paste(currentYearLandings1Q,"canum.txt",sep=''),skip=7,dec='.')
canumQ1 <- as.numeric(t(gsub(',', '', t(canumQ1))))/1000
canumQ1 <- c(0, canumQ1, 0, 0) #missing age 0 

wecaQ1_dat <- read.table(paste(currentYearLandings1Q,"weca.txt",sep=''),skip=7,dec='.')	/1000
wecaQ1 <- rep(0, 11)
wecaQ1[3:10] <- unlist(wecaQ1_dat) # adding 0 for missing ages
wecaQ1[wecaQ1 == 0] <- NA #missing age 0 and 1

## Discards
catonQ1_D <- read.table(paste(currentYearDiscards1Q,"caton.txt",sep=''),skip=7,dec='.')
catonQ1_D$V1 <- as.numeric(gsub(',', '', catonQ1_D$V1))

canumQ1_D <- read.table(paste(currentYearDiscards1Q,"canum.txt",sep=''),skip=7,dec='.')
canumQ1_D <- as.numeric(t(gsub(',', '', t(canumQ1_D))))/1000
canumQ1_D <- c(canumQ1_D, 0) #missing age 0

wecaQ1_D_dat <- read.table(paste(currentYearDiscards1Q,"weca.txt",sep=''),skip=7,dec='.')  /1000
wecaQ1_D <- rep(0, 11)
wecaQ1_D[1:10] <- unlist(wecaQ1_D_dat) #missing age 9 and 10
wecaQ1_D[wecaQ1_D == 0] <- NA #missing age 9 and 10

## Belgian TBB Q1 data in IC or not  ====
TBB_BE_Q <- F # Belgium has submitted TBB catches with quaterly precision

if(!TBB_BE_Q){ 
  ## Extract final export files from IC 
  
  taf.unzip(paste0("bootstrap/data/Q1_TBB_BE_Landings_", assyear, ".zip"), exdir=paste0("bootstrap/data/Q1_TBB_BE_landings_", assyear))
  taf.unzip(paste0("bootstrap/data/Q1_TBB_BE_Discards_", assyear, ".zip"), exdir=paste0("bootstrap/data/Q1_TBB_BE_discards_", assyear))
  
  ## Read landings and discards of Belgian TBB Q1
  currentYearLandingsTBB_BE_L_Q1 <- paste0("bootstrap/data/Q1_TBB_BE_landings_", assyear)
  currentYearLandingsTBB_BE_D_Q1 <- paste0("bootstrap/data/Q1_TBB_BE_discards_", assyear)
  
  ## Landings
  catonQ1_be <- read.table(paste(currentYearLandingsTBB_BE_L_Q1,"/caton.txt",sep=''),skip=7,dec='.')
  catonQ1_be$V1 <- as.numeric(gsub(',', '', catonQ1_be$V1))
  
  canumQ1_be <- read.table(paste(currentYearLandingsTBB_BE_L_Q1,"/canum.txt",sep=''),skip=7,dec='.')
  canumQ1_be <- as.numeric(t(gsub(',', '', t(canumQ1_be))))/1000
  canumQ1_be <- c(0, 0, canumQ1_be) 
  
  wecaQ1_be_dat <- read.table(paste(currentYearLandingsTBB_BE_L_Q1,"/weca.txt",sep=''),skip=7,dec='.')	/1000
  wecaQ1_be <- rep(0, 11)
  wecaQ1_be[3:11] <- unlist(wecaQ1_be_dat) 
  wecaQ1_be[wecaQ1_be == 0] <- NA 
  
  ## Discards
  catonQ1_D_be <- read.table(paste(currentYearLandingsTBB_BE_D_Q1,"/caton.txt",sep=''),skip=7,dec='.')
  catonQ1_D_be$V1 <- as.numeric(gsub(',', '', catonQ1_D_be$V1))
  
  canumQ1_D_be <- read.table(paste(currentYearLandingsTBB_BE_D_Q1,"/canum.txt",sep=''),skip=7,dec='.')
  canumQ1_D_be <- as.numeric(t(gsub(',', '', t(canumQ1_D_be))))/1000
  canumQ1_D_be <- c(0, canumQ1_D_be) 
  
  wecaQ1_D_be_dat <- read.table(paste(currentYearLandingsTBB_BE_D_Q1,"/weca.txt",sep=''),skip=7,dec='.')  /1000
  wecaQ1_D_be <- rep(0, 11)
  wecaQ1_D_be[2:11] <- unlist(wecaQ1_D_be_dat) 
  wecaQ1_D_be[wecaQ1_D_be == 0] <- NA 
  
  # GLOBAL Q1 objects
  catonQ1_all <- catonQ1 + catonQ1_be
  canumQ1_all <- canumQ1 + canumQ1_be
  wecaQ1_all <- (canumQ1*wecaQ1 + canumQ1_be*wecaQ1_be)/(canumQ1 + canumQ1_be)
  
  catonQ1_alldis <- catonQ1_D + catonQ1_D_be
  canumQ1_alldis <- canumQ1_D + canumQ1_D_be
  wecaQ1_alldis <- (canumQ1_D*wecaQ1_D + canumQ1_D_be*wecaQ1_D_be)/(canumQ1_D + canumQ1_D_be)
  wecaQ1_alldis[8] <- wecaQ1_D[8] #no weca for age 7 from TBB_BE
} else {
  catonQ1_all <- catonQ1
  canumQ1_all <- canumQ1
  wecaQ1_all <- wecaQ1
  
  catonQ1_alldis <- catonQ1_D
  canumQ1_alldis <- canumQ1_D
  wecaQ1_alldis <- wecaQ1_D
}

## stock Q1 just created to use sweep function to adjust sop
stockQ1 <- stock
stockQ1@landings[,ac(assyear)] <- catonQ1_all$V1
stockQ1@landings.n[,ac(assyear)] <- canumQ1_all
stockQ1@landings.wt[,ac(assyear)] <- as.numeric(wecaQ1_all)
stockQ1@discards[,ac(assyear)] <- catonQ1_alldis$V1
stockQ1@discards.n[,ac(assyear)] <- canumQ1_alldis
stockQ1@discards.wt[,ac(assyear)] <- as.numeric(wecaQ1_alldis)
sop(stockQ1,"landings")
sop(stockQ1,"discards")

##  Process data

## SOP correction
soplan <- sop(stock,"landings")
soplan
stock@landings.wt <- sweep(stock@landings.wt,2,soplan,"/")
sopc  <- sop(stock,"landings")

sopdis <- sop(stock,"discards")
sopdis
stock@discards.wt <- sweep(stock@discards.wt,2,sopdis,"/")
sopf  <- sop(stock,"discards")

## SOP correction Q1
soplanQ1 <- sop(stockQ1,"landings")   
stockQ1@landings.wt <- sweep(stockQ1@landings.wt,2,soplanQ1,"/")
sop(stockQ1,"landings")

soplanQ1 <- sop(stockQ1,"discards")   
stockQ1@discards.wt <- sweep(stockQ1@discards.wt,2,soplanQ1,"/")
sop(stockQ1,"discards")

## remove % from stock for the last year 
last_year <- dim(stock@discards)[2]
stock@landings.n[,last_year] <- stock@landings.n[,last_year] - PERCENT_7d_Q1 * stockQ1@landings.n[,last_year] *  stock@mat[,last_year]  # remove 65% of mature fish
stock@landings[,last_year]   <-  stock@landings[,last_year] - FLQuant(matrix((PERCENT_7d_Q1 * colSums((stockQ1@landings.n[,last_year]) *  stock@mat[,last_year] * stockQ1@landings.wt[,last_year], na.rm=T)), ncol=1))

stock@discards.n[,last_year] <- stock@discards.n[,last_year]  - PERCENT_7d_Q1 * stockQ1@discards.n[,last_year] *  stock@mat[,last_year]
stock@discards[,last_year] <- stock@discards[,last_year]  - FLQuant(matrix((PERCENT_7d_Q1 * colSums((stockQ1@discards.n[,last_year]) *  stock@mat[,last_year] * stockQ1@discards.wt[,last_year], na.rm=T)), ncol=1))

## SOP correction final - need to be done again for final year
soplan <- sop(stock,"landings")
soplan
stock@landings.wt <- sweep(stock@landings.wt,2,soplan,"/")
sopc  <- sop(stock,"landings")
sopc

sopdis <- sop(stock,"discards")
sopdis
stock@discards.wt <- sweep(stock@discards.wt,2,sopdis,"/")
sopf  <- sop(stock,"discards")
sopf

save(stock, file=paste0("data/stockobject_", assyear, ".RData"))

## Write files
######### save landings and N at age removed from Q1

## North Sea mat individual from Q1
TotRemovalsNS   <- FLQuant(matrix((0.5 * colSums((stockQ1@landings.n[,last_year]) * stock@mat[,last_year] * stockQ1@landings.wt[,last_year], na.rm=T)), ncol=1))
TotRemovalsN_NS <- 0.5 * stockQ1@landings.n[,last_year] *  stock@mat[,last_year]    
#TotRemovalsN_NS <- TotRemovalsN_NS[2:11,]

TotRemovalsNS_discards   <- FLQuant(matrix((0.5 * colSums((stockQ1@discards.n[,last_year]) *  stock@mat[,last_year] * stockQ1@discards.wt[,last_year], na.rm=T)), ncol=1))
TotRemovalsN_NS_discards <- 0.5 * stockQ1@discards.n[,last_year]  *  stock@mat[,last_year]    
#TotRemovalsN_NS_discards <- TotRemovalsN_NS_discards[2:11,]

## VIIe mat individual from Q1
TotRemovalsVIIe   <- FLQuant(matrix((0.15 * colSums((stockQ1@landings.n[,last_year]) * stock@mat[,last_year] * stockQ1@landings.wt[,last_year], na.rm=T)), ncol=1))
TotRemovalsN_VIIe <- 0.15 * stockQ1@landings.n[,last_year] *  stock@mat[,last_year]                        
#TotRemovalsN_VIIe <- TotRemovalsN_VIIe[2:11,]

TotRemovalsVIIe_discards   <- FLQuant(matrix((0.15 * colSums((stockQ1@discards.n[,last_year]) *  stock@mat[,last_year] * stockQ1@discards.wt[,last_year], na.rm=T)), ncol=1))
TotRemovalsN_VIIe_discards <- 0.15 * stockQ1@discards.n[,last_year]  *  stock@mat[,last_year]                    
#TotRemovalsN_VIIe_discards <- TotRemovalsN_VIIe_discards[2:11,]

TotRemovals <- TotRemovalsNS + TotRemovalsVIIe #only for landings)
TotRemovals_discards <- TotRemovalsNS_discards + TotRemovalsVIIe_discards

CatchRemovalVIIe <- TotRemovalsVIIe + TotRemovalsVIIe_discards
CatchRemovalNS <- TotRemovalsNS + TotRemovalsNS_discards

Q1.landings.wt <- stockQ1@landings.wt[,last_year]
Q1.discards.wt <- stockQ1@discards.wt[,last_year]

#Data required for assessment of ple.27.420 and ple VIIe
write.table(as.data.frame(Q1.landings.wt), paste0("data/Q1_weca_landings_", assyear,".txt"), sep=';',row.names=F)
write.table(as.data.frame(Q1.discards.wt), paste0("data/Q1_weca_discards_", assyear, ".txt"), sep=';',row.names=F)

#Data required for assessment of ple VIIe
write.table(as.matrix(TotRemovalsVIIe@.Data), paste0("data/TotRemovalsFirstQVIIe_landings_", assyear, ".txt"), sep=';',row.names=F)
write.table(as.data.frame(TotRemovalsN_VIIe@.Data),paste0("data/TotRemovalsN_FirstQVIIe_landings_", assyear, ".txt"), sep=';',row.names=F)

write.table(as.matrix(TotRemovalsVIIe_discards@.Data), paste0("data/TotRemovalsFirstQVIIe_discards_", assyear, ".txt"), sep=';',row.names=F)
write.table(as.data.frame(TotRemovalsN_VIIe_discards@.Data), paste0("data/TotRemovalsN_FirstQVIIe_discards_", assyear, ".txt"), sep=';',row.names=F)

#Data required for 2022 assessment of ple.27.420
write.table(as.matrix(TotRemovalsNS@.Data), paste0("data/TotRemovalsFirstQNS_", assyear, ".txt"), sep=';',row.names=F)
write.table(as.data.frame(TotRemovalsN_NS@.Data), paste0("data/TotRemovalsN_FirstQNS_", assyear, ".txt"), sep=';',row.names=F)

write.table(as.matrix(TotRemovalsNS_discards@.Data), paste0("data/TotRemovalsFirstQNS_discards_", assyear, ".txt"), sep=';',row.names=F)
write.table(as.data.frame(TotRemovalsN_NS_discards@.Data), paste0("data/TotRemovalsN_FirstQNS_discards_", assyear, ".txt"), sep=';',row.names=F)

save(TotRemovals, TotRemovals_discards,file= paste0("data/Q1_removals_", assyear, ".Rdata"))

## Modified stock data
units(stock)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))
range(stock)["minfbar"] <- 3
range(stock)["maxfbar"] <- 6
stock <- trim(stock, age=1:10)
stock@catch.n <- stock@landings.n  # temporary, to setPlusGroup weights
stock <- setPlusGroup(stock, 7)

## Get survey data
indices <- readFLIndices("bootstrap/data/PLE7DFleet_GAM_1993_2023.txt", na.strings="NA")
lapply(indices, summary) 

## Prepare survey data for plots
## setting age and years ranges to use for each tuning fleet 
##first age         last age          first year           last year
UK.BTS.a1 <- 1;   UK.BTS.a2<-6 ;  UK.BTS.y1 <- 1989;   UK.BTS.y2<-assyear;
FR.GFS.a1 <- 1;   FR.GFS.a2<-6 ;  FR.GFS.y1 <- 1993;   FR.GFS.y2<-assyear;    
IN.YFS.a1 <- 1;   IN.YFS.a2<-1 ;  IN.YFS.y1 <- 1987;   IN.YFS.y2<-2006;

idxcrop <- FLIndices(trim(indices[[1]], age=UK.BTS.a1:UK.BTS.a2, year=UK.BTS.y1:(UK.BTS.y2)), 
                     trim(indices[[2]], age=FR.GFS.a1:FR.GFS.a2, year=FR.GFS.y1:(FR.GFS.y2)),
                     trim(indices[[3]], age=IN.YFS.a1:IN.YFS.a2, year=IN.YFS.y1:(IN.YFS.y2)))

indsN01 <- FLQuants(lapply( mcf( lapply(idxcrop, index)), function(x){x <- FLQuant(aperm(apply(x@.Data, c(1,3,4,5,6), scale),c(2,1,3,4,5,6)), dimnames= dimnames(x))}))
names(indsN01)   <- names(indices[1:3])

## Prepare survey data for assessment
indices <- FLIndices(indices[[1]], trim(indices[[2]], age=1:6),indices[[3]])

## Extract tables
landings.n <- flr2taf(stock@landings.n)
landings.wt <- flr2taf(stock@landings.wt); landings.wt[landings.wt==0] <- NA
discards.n <- flr2taf(stock@discards.n)
discards.wt <- flr2taf(stock@discards.wt); discards.wt[discards.wt==0] <- NA
stock.wt <- flr2taf(stock@stock.wt); stock.wt[stock.wt==0] <- NA
survey.uk <- flr2taf(indices[[1]]@index)
survey.fr <- flr2taf(indices[[2]]@index)

## Rename plus group
landings.n <- plus(landings.n)
landings.wt <- plus(landings.wt)
discards.n <- plus(discards.n)
discards.wt <- plus(discards.wt)
stock.wt <- plus(stock.wt)

## Write tables to data directory
setwd("data")
write.taf(landings.n, "latage.csv")      # 2.3.1
write.taf(landings.wt, "wlandings.csv")  # 2.4.1
write.taf(discards.n, "datage.csv")      # 2.3.2
write.taf(discards.wt, "wdiscards.csv")  # 2.4.2
write.taf(stock.wt, "wstock.csv")        # 2.4.3
write.taf(survey.uk, "survey_uk.csv")    # 2.6.1a
write.taf(survey.fr, "survey_fr.csv")    # 2.6.1b
setwd("..")

## Write model input files
control <- FLAAP.control(pGrp=1, qplat.surveys=5, qplat.Fmatrix=6, Fage.knots=4,
                         Ftime.knots=14, Wtime.knots=5, mcmc=FALSE)

# already done in model.R
# path <- "data"  # required inside assessment() function
# suppressWarnings(assessment(stock, indices, control, input=TRUE, model=FALSE))
save(control, indices, indsN01, stock, file=paste0("data/input_", assyear, ".RData"))

# for the forecast
resid_catch <- stock@landings + stock@discards

# for the advice sheet
obs_lan <- as.data.frame(stock@landings)
obs_dis <- as.data.frame(stock@discards)

write.csv(obs_lan, "report/observed landings.csv")
write.csv(obs_dis, "report/observed discards.csv")
