# Build your analysis for stock assessment 
# GH 2023


rm(list=ls(all=TRUE))

library(devtools)
install_github("flr/FLCore@e0c02e5")
install_github("flr/FLash@7c47560")
install_github("flr/FLAssess@c25d7a5")
install_github("flr/FLEDA@f9f2c77")

# install.packages("icesTAF")
library(icesTAF)
# setwd("C:/Users/ghalouan/Work/IFREMER/Expertise/Stock_assessment_Ghassen/WGNSSK_2020/plaice7d_assessment/2020_ple.27.7d_assessment")

#build analysis folder
taf.skeleton()

#After adding initial data files into bootstrp/initial/data (fleet + stockObject)
#build DATA.bib
draft.data()
draft.data(originator="WGNSSK", year="2019", file="bootstrap/DATA.bib")
draft.data(year="2019")
draft.data(originator="WGNSSK", year="2019", title = c("Official landings, TAC, Q1 removals from assessement", "outputs from 2019 assessment of ple.27.7d"), period = c("1987-2017", "1980-2017"), file="bootstrap/DATA.bib")
getwd()
# load("bootstrap/initial/data/stockobject.Rdata")

#build bootstrap/data folder and store it
taf.bootstrap()

# build SOFTWARE.bib
draft.software(c("FLCore","FLash","FLAssess","FLXSA"), file = "bootstrap/SOFTWARE.bib")
taf.bootstrap()
