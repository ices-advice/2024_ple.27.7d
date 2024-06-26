## Prepare tables for report

## Before: latage.csv, wlandings.csv, datage.csv, wdiscards.csv, wstock.csv,
##         survey_uk.csv, survey_fr.csv (data), res_discards.csv,
##         summary.csv (output)
## After:  latage.csv, wlandings.csv, datage.csv, wdiscards.csv, wstock.csv,
##         survey_uk.csv, survey_fr.csv (data), res_discards.csv,
##         summary.csv (report)

library(icesTAF)

## latage (round)
latage <- read.taf("data/latage.csv")
latage <- round(latage)
write.taf(latage, "report/latage.csv")

## wlandings (round)
wlandings <- read.taf("data/wlandings.csv")
wlandings <- round(wlandings, 3)
write.taf(wlandings, "report/wlandings.csv")

## datage (trim year, round)
datage <- read.taf("data/datage.csv")
datage <- na.omit(datage)
datage <- round(datage)
write.taf(datage, "report/datage.csv")

## wdiscards (trim year, round)
wdiscards <- read.taf("data/wdiscards.csv")
wdiscards <- wdiscards[rowSums(wdiscards[-1],na.rm=TRUE) > 0,]
wdiscards <- round(wdiscards, 3)
write.taf(wdiscards, "report/wdiscards.csv")

## wstock (round)
wstock <- read.taf("data/wstock.csv")
wstock <- round(wstock, 3)
write.taf(wstock, "report/wstock.csv")

## survey_uk (round)
survey_uk <- read.taf("data/survey_uk.csv")
survey_uk <- round(survey_uk, 1)
write.taf(survey_uk, "report/survey_uk.csv")

## survey_fr (round)
survey_fr <- read.taf("data/survey_fr.csv")
survey_fr <- round(survey_fr, 1)
write.taf(survey_fr, "report/survey_fr.csv")

## res_discards (trim year)
res_discards <- read.taf("output/res_discards.csv")
res_discards <- na.omit(res_discards)
write.taf(res_discards, "report/res_discards.csv")

## summary (select columns, round)
summary <- read.taf("output/summary.csv")
summary <- summary[c("Year","SSB","Catch","Landings","Biomass","Fbar")]
summary <- rnd(summary, c("SSB","Catch","Landings","Biomass"))
summary <- rnd(summary, "Fbar", 5)
names(summary)[names(summary)=="Biomass"] <- "Total Biomass"
write.taf(summary, "report/summary.csv")

# ### ------------------------------------------------------------------------------------------------------
# ###    Print tables from 3. indices.R
# ### ------------------------------------------------------------------------------------------------------
# setwd(result.path)
# write.wg.txt.indices(number= "indices 1 and 2", x=FLIndices(indices[[1]],indices[[2]]),filename="indices.txt", name="Tuning data. BTS and GFS surveys", round=1)
# write.wg.txt.indices(number= "indice 3", x=FLIndices(indices[[3]]),filename="indices_cont.txt", name="IN YFS",  round=2)


## Tables for the report GH
library(tidyr)
load("output/output.RData")
Lan_n <- ass.stock@landings.n
Lan_n_df <- as.data.frame(Lan_n)
Lan_n_w <- pivot_wider(Lan_n_df, names_from = age, values_from = data)
write.csv(Lan_n_w, "output/Landings_number_ass.stock.csv")
