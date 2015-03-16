complete <- function(directory, ids = 1:332) {
  res<-data.frame(id=ids, nobs=0)
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
    s<-is.na(fdata["sulfate"])
    n<-is.na(fdata["nitrate"])
    res[i,2]<-length(fdata[1][!s&!n])
    s<-NA
    n<-NA
    fdata<-NA
  }
  return (res)
}


