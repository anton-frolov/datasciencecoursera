pollutantmean <- function(directory, pollutant, ids = 1:332) {
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
  rids<-c(sid:eid)
  vres<-vector(mode="numeric", length=length(rids))
  for(id in rids){
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
    vpol<-fdata[[pollutant]]
    vres[id-sid+1]<-mean(vpol,na.rm=TRUE)
    fdata<-NA
  }
  return (mean(vres))
}