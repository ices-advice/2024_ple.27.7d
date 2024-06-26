# Read in raw data from IC
################################## readStockOverview ###############################

readStockOverview <- function(StockOverviewFile,NumbersAtAgeLengthFile){
  
  Wdata <- read.table(StockOverviewFile,header=TRUE,sep="\t")
  # Enlever la col unit eff qui n'?tait pas l? avant
  Wdata <- Wdata[,-9]
  names(Wdata)[7] <- "Fleet"
  names(Wdata)[9] <- "CatchWt"
  names(Wdata)[10] <- "CatchCat"
  names(Wdata)[11] <- "ReportCat"
  Wdata$CatchCat <- substr(Wdata$CatchCat,1,1)
  Wdata <- Wdata[,-ncol(Wdata)]
  
  Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
  names(Ndata)[7] <- "CatchCat"
  names(Ndata)[9] <- "Fleet"
  
  Wdata <- merge(Wdata,Ndata[,c(3,4,5,7,9,10,11)],by=c("Area","Season","Fleet","Country","CatchCat"),all.x=TRUE)
  Wdata$Sampled <- ifelse(is.na(Wdata$SampledCatch),FALSE,TRUE)
  
  return(Wdata)
}

################################## readNumbersAtAgeLength ###############################

readNumbersAtAgeLength <- function(NumbersAtAgeLengthFile){
  
  Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
  names(Ndata)[7] <- "CatchCat"
  names(Ndata)[8] <- "ReportCat"
  names(Ndata)[9] <- "Fleet"
  Ndata <- Ndata[,-ncol(Ndata)]
  for(i in 16:(16+ages_1-1)){
    Ndata[i]<- Ndata[i]+Ndata[(i+ages_1)]+Ndata[(i+2*ages_1)]
    names(Ndata)[i] <- substr(names(Ndata)[i],8,nchar(names(Ndata)[i]))
  }
  Ndata <- Ndata[1:(16+ages_1-1)]
  return(Ndata)
}