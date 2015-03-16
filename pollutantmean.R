pollutantmean <- function(directory, pollutant, ids = 1:332) {
  vres<-vector(mode="numeric")
  i<-0
  for(id in ids){
    i<-i+1
    fname<-as.character(id)
    if(id<10){
      fname<-paste("00",fname,sep="")
    }else{
      if(id>9&id<100){
        fname<-paste("0",fname,sep="")
      }
    }
    fname<-paste(fname,"csv",sep=".") 
    fname<-paste(directory,fname,sep="/")
    fdata<-read.csv(fname,header=TRUE,sep=",")
    p<-is.na(fdata[[pollutant]])
    vpol<-fdata[[pollutant]]
    vres<-c(vres, vpol)
    fdata<-NA
  }
  return (mean(vres,na.rm=TRUE))
}