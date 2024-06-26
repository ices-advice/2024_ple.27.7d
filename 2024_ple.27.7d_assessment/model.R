## Run analysis, write model results

## Before: sole (bootstrap/software), input.RData (data)
## After:  input.RData, results.RData, assess.dat, sole.rep, sole.std (model)
setwd("..")
library(icesTAF)#3.0.0
suppressMessages(taf.library(FLAssess))
library(splines)
suppressMessages(library(mgcv))
library(methods)
source("utilities_model.R")

mkdir("model")

## Get model executable
exefile <- if(os.linux()) "sole" else "sole.exe"
taf.unzip("bootstrap/software/sole.zip", files=exefile, exdir="model")

## Get model input files
load("data/input_2023.RData")


minyear <- range(stock)[["minyear"]]
maxyear <- range(stock)[["maxyear"]]
## Run model
path <- "model"  # required inside assessment() function

## Build new initial condition for the model using only UK BTS indices from 1989-2015 and full FR GFS time series or model is not converging at first
indices.assess <- FLIndices(trim(indices[[1]],year=c(1989:2020)),
                             trim(indices[[2]],age=1:6, year=1993:(maxyear-8)))

## run the model to build initial parameters
suppressWarnings(results <-
                   assessment(stock, indices.assess, control, addargs= "", model=TRUE))

## Copy parameter estimates by the model to build new initial condition
cp("model/sole.par", "model/plaice.pin")

indices.assess <- FLIndices(trim(indices[[1]],year=c(1989:(maxyear-0))),
                            trim(indices[[2]],age=1:6, year=1993:(maxyear-0)))

## run the model with the full time series of survey indices
suppressWarnings(results <-
                   assessment(stock, indices.assess, control, addargs= " -ainp plaice.pin", model=TRUE))

save(results, indices.assess, file="model/results.RData")

## Run retrospective analysis with discard (min year to update)
retro.dat <- retrospective_alt(stock, indices.assess, control, (maxyear-5):maxyear, path, addargs= " -ainp plaice.pin")

save(retro.dat, file="model/retro.RData")

