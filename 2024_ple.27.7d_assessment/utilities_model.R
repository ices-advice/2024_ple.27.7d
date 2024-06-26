suppressMessages(library(FLAssess))
library(methods)

setClass("FLAAP.control",
         representation(
           pGrp         ="logical",
           qplat.surveys="integer",
           qplat.Fmatrix="integer",
           Fage.knots   ="integer",
           Ftime.knots  ="integer",
           Wtime.knots  ="integer",
           mcmc         ="logical"),
         prototype=prototype(
           pGrp         =TRUE,
           qplat.surveys=as.integer(5),
           qplat.Fmatrix=as.integer(5),
           Fage.knots   =as.integer(5),
           Ftime.knots  =as.integer(5),
           Wtime.knots  =as.integer(7),
           mcmc         =FALSE)
)

setClass("FLAAP",
         contains='FLAssess',
         representation(
           landings.wt  ="FLQuant",
           landings.n   ="FLQuant",
           landings.var ="FLQuant",
           discards.wt  ="FLQuant",
           discards.n   ="FLQuant",
           discards.var ="FLQuant",
           stock.wt     ="FLQuant",
           q.hat        ="FLQuants",
           stdfile      ="data.frame",
           control      ="FLAAP.control"),
         prototype=prototype(
           landings.wt  =FLQuant(),
           landings.n   =FLQuant(),
           landings.var =FLQuant(),
           discards.wt  =FLQuant(),
           discards.n   =FLQuant(),
           discards.var =FLQuant(),
           stock.wt     =FLQuant(),  
           q.hat        =FLQuants(),
           stdfile      =data.frame(),
           control      =new("FLAAP.control"))
)

FLAAP.control <- function(x=NULL, pGrp=TRUE, qplat.surveys=5, qplat.Fmatrix=5, Fage.knots=5, Ftime.knots=5,Wtime.knots=7, mcmc=F){
  if (is.null(x)){
    res <- new("FLAAP.control",  pGrp=as.logical(pGrp)[1], qplat.surveys=as.integer(qplat.surveys),qplat.Fmatrix=as.integer(qplat.Fmatrix), Fage.knots=as.integer(Fage.knots),
               Ftime.knots=as.integer(Ftime.knots),Wtime.knots=as.integer(Wtime.knots), mcmc=as.logical(mcmc)[1])
  } else {	# We reuse an FLXSA.control object embedded in an FLXSA object
    if (!is.null(x) & !( is(x, "FLAAP.control")))
      stop("FLAAP must be an 'FLAAP.control' object!")
    
    if (is(x, "FLAAP.control"))
      res <- x
  } 
  return(res)
}

#makeDAT <- function(stock, numYr,qplat,F_age_knots,F_time_knots, W_time_knots, numAges, pGrp, indMPs, selSplines, Fspline, Wspline, tquants){
makeDAT <- function(stock, numYr,qplat_Fmatrix,qplat_surveys,F_age_knots,F_time_knots, W_time_knots, numAges, pGrp, indMPs, selSpline, X, WSpline, tquants){
  cat("#############\n")
  cat("# ",name(stock),"\n# Created:",format(Sys.time(), "%d%b%Y_%Hh%M"),"\n")
  cat("# years:",range(stock)["minyear"],"-",range(stock)["maxyear"]," ; ages:",range(stock)["min"],"-",range(stock)["max"],"; q plateau; Fbar range", range(stock)["minfbar"],"-", range(stock)["maxfbar"], "; number of knots in time spline \n")
  cat(numYr,numAges,qplat_Fmatrix,qplat_surveys, range(stock)["minfbar"],range(stock)["maxfbar"], F_age_knots, F_time_knots, W_time_knots, as.integer(pGrp),"\n")
  cat(length(indMPs), unlist(indMPs))
  cat("\n#############\n")
  
  for (ii in 1:length(tquants)){
    write.table(tquants[[ii]], row.names=F, col.names=F,quote=F)
    cat("\n");
  }
  
  landings(stock)[,!apply(is.na(landings.n(stock)),2,all)] <- -1 # if landings at age available then set landings to -1 so not used
  landings(stock)[is.na(landings(stock))] <- -1 # if landings at age available then set landings to -1 so not used
  
  cat("#############\n# landings \n",landings(stock),"\n")
  
  cat("#############\n# M \n", m(stock)[1,1],"\n")
  
  cat("#############\n# Maturity (females) \n")
  tmp <- matrix(mat(stock)[,1]@.Data,ncol=1)
  tmp[is.na(tmp)] <- round(-1,0)
  write.table(tmp, row.names=F, col.names=F,quote=F)
  
  cat("#############\n# Selectivity spline (surveys): F_age_knot knots, qplat_surveys ages (last ages are equal)","\n")
  write.table(selSpline, row.names=F, col.names=F,quote=F)
  
  # cat("#############\n# Annual F spline","\n")
  # write.table(Fspline, row.names=F, col.names=F,quote=F)
  
  cat("#############\n# tensor spline design matrix","\n")
  write.table(X, row.names=F, col.names=F,quote=F)
  
  cat("#############\n# Annual W spline","\n")
  write.table(WSpline, row.names=F, col.names=F,quote=F)
  
} # end of function


assessment <- function(stock, indices, control, addargs=" ", input=TRUE, model=TRUE){
  # input: whether to write *.dat input file
  # model: whether to run model and read output files
  
  #TODO check if survey age ranges extend stock: that is not possible and results in incorrect results 
  
  #get info from control
  pGrp           <- control@pGrp
  qplat_surveys  <- control@qplat.surveys
  qplat_Fmatrix  <- control@qplat.Fmatrix  
  F_age_knots    <- control@Fage.knots
  F_time_knots   <- control@Ftime.knots
  W_time_knots   <- control@Wtime.knots
  
  # Number of years
  years <- as.numeric(range(stock)["minyear"]:range(stock)["maxyear"])
  numYr <- length(years)
  
  # Number of ages
  numAges <- length(1:range(stock)[["max"]])
  
  # indices 
  indexVals  <- lapply(indices, index)
  numIndices <- length(indexVals)
  
  # index midpoints (timing during year)
  indMPs <- list()
  for (ind in names(indices)) 
    indMPs[[ind]] <- as.numeric((range(indices[[ind]])["startf"]+range(indices[[ind]])["endf"])/2)
  
  ### ------------------------------------------------------------------------------------------------------
  ###   3. Calculate splines
  ### ------------------------------------------------------------------------------------------------------
  # selectivity surveys
  selSpline <- format(t(matrix(bs(1:qplat_surveys,F_age_knots,intercept=T),ncol=F_age_knots)),nsmall=9)
  
  
  #USE TENSOR spline instead
  # now make design matrix for F over ages and time, and for U1
  X  <- gam(dummy ~ te(age, year, k = c(F_age_knots,F_time_knots)), data = expand.grid(dummy = 1, age = 1:qplat_Fmatrix, year = as.numeric(1:numYr)), fit = FALSE) $ X
  
  # Annual W
  WSpline <- format(t(matrix(bs(1:numYr,df=W_time_knots,intercept=T),ncol=W_time_knots)),nsmall=9)
  
  ### ------------------------------------------------------------------------------------------------------
  ### 4. generate equally sized tquants that can be written to disk
  ### ------------------------------------------------------------------------------------------------------
  quants <- mcf(c(list(landings.n(stock), discards.n(stock),landings.wt(stock),discards.wt(stock),stock.wt(stock)), indexVals))
  for (ii in 1:length(quants)){
    if (!(ii %in% c(3,4,5))){ quants[[ii]] <- quants[[ii]] + min(quants[[ii]][!quants[[ii]]==0], na.rm=T)/2}
    quants[[ii]][is.na(quants[[ii]])] <- round(-1,0)
  }
  
  tquants <- lapply(quants,function(x){x <- matrix(x,nrow=dim(x)[1]);t(x);})
  
  ### ------------------------------------------------------------------------------------------------------
  ###   5. Create .dat file
  ### ------------------------------------------------------------------------------------------------------ 
  # capture.output(makeDAT(stock, numYr, qplat,F_age_knots,F_time_knots,W_time_knots, numAges, pGrp, indMPs, selSplines, Fspline, Wspline, tquants), file=paste(path,"\\assess",".dat",sep=""))
  if (input)
  {
    capture.output(makeDAT(stock, numYr, qplat_Fmatrix,qplat_surveys,F_age_knots,F_time_knots,W_time_knots, numAges, pGrp, indMPs, selSpline, X, WSpline, tquants), file=paste(path,"/assess",".dat",sep=""))
  }
  
  ### ------------------------------------------------------------------------------------------------------
  ###   6. Run model & read output files
  ### ------------------------------------------------------------------------------------------------------
  if (model)
  {
    capture.output(makeDAT(stock, numYr, qplat_Fmatrix,qplat_surveys,F_age_knots,F_time_knots,W_time_knots, numAges, pGrp, indMPs, selSpline, X, WSpline, tquants), file=paste(path,"/assess",".dat",sep=""))
    oldPath <- getwd()
    setwd(path)  
    
    modName  <- "sole"
    dmns     <- list(year=years, age=1:numAges)
    dmnsiter <- list(age=dmns[[2]],year=dmns[[1]],iter=1:1000)
    nyears   <- length(dmns[[1]])
    res     <- new("FLAAP")
    
    if (control@mcmc==F){ 
      if (file.exists("sole.std")) system("rm sole.std")
      system(paste("./sole -nox -ind assess.dat", addargs, sep=""))
      #First see if std file exists. If not: trouble
      if (file.exists(paste(modName,".std",sep=""))){
        repFull <- readLines(paste(modName,".rep",sep=""),n=-1)
        stdfile <- readLines(paste(modName,".std",sep=""))
        
      } else{
        stop("Hessian not positive definite?")
      }
      # even if std file exists, std estimates may be lacking, also trouble :)
      if (rev(unlist(strsplit(stdfile[2]," ")))[1] %in% c("1.#QNBe+000","-1.#INDe+000" )){
        stop("Hessian not positive definite?")
      }
      
      res@stdfile <- read.table("sole.std", skip=1, col.names= c("index","name","mean","stddev"))
      
      estN       <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated N"), nrow=nyears)
      estF       <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated F"), nrow=nyears)
      estSWT     <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated SWT"), nrow=nyears)
      
      res@stock.n  <- as.FLQuant(t(matrix(data.matrix(estN),nrow=nyears, dimnames=dmns)))
      res@harvest  <- as.FLQuant(t(matrix(data.matrix(estF),nrow=nyears, dimnames=dmns)))
      res@stock.wt <- as.FLQuant(t(matrix(data.matrix(estSWT),nrow=nyears, dimnames=dmns)))
      
    } else if (control@mcmc== T){
      system(paste("./sole -ind assess.dat -mcmc 1e5 -mcsave 1e2", addargs, sep=""))
      system("./sole -ind assess.dat -mceval") 
      repFull <- readLines(paste(modName,".rep",sep=""),n=-1)
      stdfile <- readLines(paste(modName,".std",sep=""))
      
      estN <- array(unlist(read.table("N.mcmc")), dim=c(numYr,1000,numAges))
      estN <- aperm(estN,c(3,1,2))
      res@stock.n  <- as.FLQuant(c(estN), dimnames=dmnsiter)
      
      estF <- array(unlist(read.table("F.mcmc")), dim=c(numYr,1000,numAges))
      estF <- aperm(estF,c(3,1,2))
      res@harvest  <- as.FLQuant(c(estF), dimnames=dmnsiter)
      
      estSWT <- array(unlist(read.table("swt.mcmc")), dim=c(numYr,1000,numAges))
      estSWT <- aperm(estSWT,c(3,1,2))
      res@stock.wt  <- as.FLQuant(c(estSWT), dimnames=dmnsiter)
      
      
    } 
    
    ### Read in full file and stdfile
    
    estLWT    <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated LWT"), nrow=nyears)
    #estDWT    <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated DWT"), nrow=nyears)
    estTSB    <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated TSB"), nrow=1)
    estSSB    <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated SSB"), nrow=1)
    estSELF1  <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="log_self1"), nrow=1)
    estSELU   <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="log_selU"), nrow=length(indMPs))
    estLAA    <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated l@a"), nrow=nyears)
    estDAA    <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated d@a"), nrow=nyears)
    #forloopje schrijven
    
    for (ss in 1:length(indMPs) ){
      estSurv  <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="Estimated surveys")+((ss-1)*nyears), nrow=nyears)
      if (ss == 1){ 
        res@index.hat <- FLQuants(as.FLQuant(t(matrix(data.matrix(estSurv),nrow=nyears, dimnames=dmns))))
      } else {
        res@index.hat[[ss]] <- as.FLQuant(t(matrix(data.matrix(estSurv),nrow=nyears, dimnames=dmns)))
      }  
    }
    
    
    estsigmaL <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="sigmaL"), nrow=1)
    estsigmaD <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="sigmaD"), nrow=1)
    estsigmaU <- read.table(paste(modName,".rep",sep=""), skip=which(repFull=="sigmaU"), nrow=length(indMPs))
    
    setwd(oldPath) 
    
    hatdmns <-list(year="all",age=dmns$age)
    
    res@landings.n   <- as.FLQuant(t(matrix(data.matrix(estLAA),nrow=nyears, dimnames=dmns)))
    res@landings.wt  <- as.FLQuant(t(matrix(data.matrix(estLWT),nrow=nyears, dimnames=dmns)))
    res@landings.var <- as.FLQuant(t(matrix(data.matrix(estsigmaL),nrow=1, dimnames=hatdmns)))
    
    res@discards.n   <- as.FLQuant(t(matrix(data.matrix(estDAA),nrow=nyears, dimnames=dmns)))
    # res@discards.wt  <- as.FLQuant(t(matrix(data.matrix(estDWT),nrow=nyears, dimnames=dmns)))
    res@discards.var <- as.FLQuant(t(matrix(data.matrix(estsigmaD),nrow=1, dimnames=hatdmns)))
    
    res@catch.n <- res@discards.n + res@landings.n 
    
    for (ss in 1:length(indMPs) ){
      if (ss == 1){ 
        res@q.hat         <- FLQuants(as.FLQuant(t(matrix(data.matrix(estSELU[1,]),nrow=1, dimnames=hatdmns))))
        res@index.res     <- FLQuants(log(quants[[6]])-log(res@index.hat[[1]]))
        res@index.var     <- FLQuants(as.FLQuant(t(matrix(data.matrix(estsigmaU[1,]),nrow=1, dimnames=hatdmns)))) 
        #set thoseages in index.var to NA that have only NA in residuals (meaning there was never any data)
        res@index.var[[1]][apply(is.na(res@index.res[[1]]),1,all)] <- NA
      } else {
        res@q.hat[[ss]]     <- as.FLQuant(t(matrix(data.matrix(estSELU[ss,]),nrow=1, dimnames=hatdmns)))
        res@index.res[[ss]] <- log(quants[[5+ss]])-log(res@index.hat[[ss]])
        res@index.var[[ss]] <- as.FLQuant(t(matrix(data.matrix(estsigmaU[ss,]),nrow=1, dimnames=hatdmns)))
        #set thoseages in index.var to NA that have only NA in residuals (meaning there was never any data)
        res@index.var[[ss]][apply(is.na(res@index.res[[ss]]),1,all)] <- NA
      }  
    }
    
    res@q.hat@names <- res@index.hat@names <- indices@names
    
    res@index <- FLQuants(indexVals)
    res@control <- control
    
    return(res)
  }
}  


YPR <- function(stock, assess){
  mat <- mat(stock)[1:maxA,ac(2013)]
  M <- 0.2
  cwts  <- apply(stock@catch.wt[,ac(2011:2013)],)
  lwts  <- stock@landings.wt[,ac(2011:2013)]
  swts  <- stock@stock.wt[,ac(2011:2013)]
  yprSel <- assess@harvest[,ac(2013)]
  Dfrac <- assess@discards.n[,ac(2013)]/ (assess@landings.n[,ac(2013)] +  assess@discards.n[,ac(2013)])
  
  vals  <- seq(0,2,0.01)
  CPR <- LPR <- Fbar <- SSB <- numeric(length(vals))
  ix <- 1
  for (ii in vals){
    Z        <- M + (yprSel * ii)
    cumZ     <- cumsum(c(Z))
    N        <- c(1, exp(-cumZ))
    Catch    <- ((yprSel * ii )/Z) * N[-length(N)] * (1-exp(-Z))
    LPR[ix]  <- sum(Catch * (1-Dfrac) * lwts)
    CPR[ix]  <- sum(Catch * cwts)
    SSB[ix]  <- sum(N[-length(N)] * swts * mat)
    Fbar[ix] <- mean(as.numeric((yprSel * ii)[2:6]))
    ix <- ix +1
  }
  return(list("Fbar"=Fbar,"CPR"=CPR,"LPR"=LPR,"SSB"=SSB))
}

doPG <- function(stock,pGrp,maxA){
  if (pGrp){
    landings.n(stock)[ac(11:12),][is.na(landings.n(stock)[ac(11:12),])] <- 0
    stkWtPgrp <- apply(stock.wt(stock)[ac(maxA:lastA),]*sweep(landings.n(stock)[ac(maxA:lastA),],2,apply(landings.n(stock)[ac(maxA:lastA),],2,sum,na.rm=T),"/"),2,sum,na.rm=T)
    stkWtPgrp[stkWtPgrp==0] <- NA
    landWtPgrp <- apply(landings.wt(stock)[ac(maxA:lastA),]*sweep(landings.n(stock)[ac(maxA:lastA),],2,apply(landings.n(stock)[ac(maxA:lastA),],2,sum,na.rm=T),"/"),2,sum,na.rm=T)
    landWtPgrp[landWtPgrp==0] <- NA
    
    # Apply max age
    stock <- setPlusGroup(stock, plusgroup=maxA,na.rm=T) 
    
    # Add plusgroup weights
    stock.wt(stock)[maxA,]    <- stkWtPgrp
    landings.wt(stock)[maxA,] <- landWtPgrp
    
    # Replace 0s in landings plusgroup with NA (were NA anyway, setPlusgroup generated 0s)
    landings.n(stock)[maxA,landings.n(stock)[maxA,]==0] <- NA 
  } else {
    stock <- trim(stock, age=1:maxA)
  }
  return(stock)
}

retrospective <- function(stock,indices,control,years,path, addargs=" "){
  resas.retro <-  list()
  ssb.retro <- fbar.retro <- R.retro <- NULL
  
  for (yrs in sort(years,T)){  
    stock.retro            <- window(stock,end=yrs)
    if (yrs == max(years)){ 
      resas.retro[[ac(yrs)]] <- try(assessment(stock.retro, window(indices, end=yrs), control, addargs=addargs))
    } else { 
      resas.retro[[ac(yrs)]] <- try(assessment(stock.retro, window(indices, end=yrs), control, addargs= " -ainp retro.pin"))
    }
    ssb.retro              <- try(rbind(ssb.retro,cbind(qname=yrs,as.data.frame(apply( stock.retro@stock.wt* resas.retro[[ac(yrs)]]@stock.n* stock.retro@mat,2,sum, na.rm=T)))))
    fbarrange              <- range(stock)[["minfbar"]]:range(stock)[["maxfbar"]]
    fbar.retro             <- try(rbind(fbar.retro,cbind(qname=yrs,as.data.frame(apply(  resas.retro[[ac(yrs)]]@harvest[ac(fbarrange)],2,mean)))))
    R.retro                <- try(rbind(R.retro,cbind(qname=yrs,as.data.frame(resas.retro[[ac(yrs)]]@stock.n[1,] ))))
    
    parfile    <- readLines("model/sole.par")
    recests    <- unlist(strsplit(parfile[which(parfile=="# log_initpop:" ) +1]," "))
    newrecests <- paste(head(recests,length(recests)-1), collapse=" ")
    newparfile <- parfile
    newparfile[which(parfile=="# log_initpop:" ) +1] <- newrecests
    cat(newparfile, sep="\n", file="model/retro.pin" )
  }
  retro.dat   <- rbind(cbind(value="SSB",ssb.retro),
                       cbind(value="Recruits",R.retro),
                       cbind(value="Mean F",fbar.retro))
  return(retro.dat)    
}

retrospective_alt <- function(stock,indices,control,years,path, addargs=" "){
  resas.retro <-  list()
  ssb.retro <- fbar.retro <- R.retro <- NULL
  
  for (yrs in sort(years,T)){  
    stock.retro            <- window(stock,end=yrs)
    if (yrs == max(years)){ 
      resas.retro[[ac(yrs)]] <- try(assessment(stock.retro, window(indices, end=yrs), control, addargs=addargs))
    } else { 
      resas.retro[[ac(yrs)]] <- try(assessment(stock.retro, window(indices, end=yrs), control, addargs= " -ainp retro.pin"))
    }
    ssb.retro              <- try(rbind(ssb.retro,cbind(qname=yrs,as.data.frame(apply( resas.retro[[ac(yrs)]]@stock.wt* resas.retro[[ac(yrs)]]@stock.n* stock.retro@mat,2,sum, na.rm=T)))))
    fbarrange              <- range(stock)[["minfbar"]]:range(stock)[["maxfbar"]]
    fbar.retro             <- try(rbind(fbar.retro,cbind(qname=yrs,as.data.frame(apply(  resas.retro[[ac(yrs)]]@harvest[ac(fbarrange)],2,mean)))))
    R.retro                <- try(rbind(R.retro,cbind(qname=yrs,as.data.frame(resas.retro[[ac(yrs)]]@stock.n[1,] ))))
    
    parfile    <- readLines("model/sole.par")
    recests    <- unlist(strsplit(parfile[which(parfile=="# log_initpop:" ) +1]," "))
    newrecests <- paste(head(recests,length(recests)-1), collapse=" ")
    newparfile <- parfile
    newparfile[which(parfile=="# log_initpop:" ) +1] <- newrecests
    cat(newparfile, sep="\n", file="model/retro.pin" )
  }
  retro.dat   <- rbind(cbind(value="SSB",ssb.retro),
                       cbind(value="Recruits",R.retro),
                       cbind(value="Mean F",fbar.retro))
  return(retro.dat)    
}

# Function used to format output from functions retrospective and retrospective_alt into input table for the mohn function of icesAdvice package
retrospective_MohnRho <- function(retro.dat, indice.name = NULL) {
  require("reshape")
  
  if(is.null(indice.name)) indice.name <- as.character(unique(retro.dat$value))
  
  if (!all(indice.name %in% as.character(unique(retro.dat$value)))) stop("At least one indice name provided is wrong please change indice.name!")
  
  retro.dat$qname <- retro.dat$qname - max(retro.dat$qname)
  retro.dat <- retro.dat[, c("value", "qname", "year", "data")]
  retro.dat$value.qname <- paste(retro.dat$value, retro.dat$qname, sep = ".")
  retro.dat <- reshape(retro.dat, v.names = "data", idvar = "value.qname", timevar = "year", direction = "wide")
  names(retro.dat) <- gsub("data.", "", names(retro.dat))
  
  MohnRho_indices <- function(retro.dat, ind.name){
    MohnRho.ind <- retro.dat[retro.dat$value == ind.name,]
    MohnRho.ind <- MohnRho.ind[,c(-1,-3)]
    MohnRho.ind <- as.data.frame(t(MohnRho.ind))
    names(MohnRho.ind) <- MohnRho.ind[1,]
    names(MohnRho.ind) <- gsub("0", "base", names(MohnRho.ind))
    MohnRho.ind <- MohnRho.ind[-1,]
    
    return(MohnRho.ind)
  }
  
  retro.MohnRho <- vector("list", length(indice.name))
  names(retro.MohnRho) <- indice.name
  names(retro.MohnRho) <- gsub(" " , "", names(retro.MohnRho))
  
  for(i in 1:length(indice.name)){
    retro.MohnRho[[i]] <- MohnRho_indices(retro.dat, indice.name[i])
  }
  return(retro.MohnRho)
}

#Mohn row calculation modified from icesAdvice mohn function add title + way to set a min year and increase size of lines
my_mohn<- function (x, peels = 5, details = FALSE, plot = FALSE, title = NULL, cut = rownames(x)[1], ...)
{
  if (is.null(peels))
    peels <- ncol(x) - 1
  if (peels > ncol(x) - 1)
    stop("peels cannot exceed number of retro columns")
  n <- nrow(x)
  retro <- rep(NA_real_, n)
  for (i in seq_len(peels)) retro[n - i] <- x[n - i, 1 + i]
  compare <- na.omit(data.frame(base = x[, 1], retro, row.names = rownames(x)))
  compare$relbias <- (compare$retro - compare$base)/compare$base
  rho <- mean(compare$relbias)
  out <- if (details)
    list(compare = compare, rho = rho)
  else rho
  if (plot) {
    x <- x[which(rownames(x) > cut),]
    matplot(as.numeric(rownames(x)),
            x,
            type = "l",
            lty = 1,
            ann = FALSE,
            lwd = 2, ylim = c(0, max(x, na.rm = T)),
            ...)
    points(as.numeric(rownames(compare)),
           compare$retro,
           ...)
    title(paste(title))
  }
  out
}