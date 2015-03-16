complete <- function(directory, ids = 1:332) {
  fnames<-list.files(directory,pattern=".csv") 
  fids<-as.integer(substr(fnames,1,3))
  if(fids[1]<ids[1]|fids[1]==ids[1]){
    sid<-ids[1]
  }else{
    sid<-fids[1]
  }
  if(fids[length(fids)]>ids[length(ids)]|fids[length(fids)]==ids[length(ids)]){
    eid<-ids[length(ids)]
  }else{
    eid<-fids[length(fids)]
  }
  rids<-ids  
  res<-data.frame(id=ids, nobs=0)
  i<-0
  for(id in rids){
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


corr <- function(directory, threshold = 0) {
  fnames<-list.files(directory,pattern=".csv") 
  fids<-as.integer(substr(fnames,1,3))
  res<-vector(mode="numeric", length=length(fids))
  i<-0
  for(id in fids){
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
    if(length(fdata[1][!s&!n])>threshold){
      res[i]<-cor.test(~sulfate+nitrate, fdata)
    }
    
  }
  nvl<-is.na(res)
  res<- res[!nvl]
  return(res)
}