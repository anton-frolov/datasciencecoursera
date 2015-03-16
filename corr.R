corr <- function(directory, threshold = 0) {
  fnames<-list.files(directory,pattern=".csv") 
  ov<-vector(mode="numeric")
  rf<-data.frame(sulfate=0, nitrate=0, ID=0)
  res<-data.frame(sulfate=0, nitrate=0, ID=0)
  for(fname in fnames){
    fname<-paste(directory,fname,sep="/")
    fdata<-read.csv(fname, header=TRUE, sep=",")
    d<-is.na(fdata["Date"])
    s<-is.na(fdata["sulfate"])
    n<-is.na(fdata["nitrate"])
    i<-is.na(fdata["ID"])
    if(nrow(fdata[!d&!s&!n&!i,][,2:4])>threshold){
      res<-fdata[!d&!s&!n&!i,][,2:4]
      sv<-res[["sulfate"]]
      nv<-res[["nitrate"]]
      ov<-c(ov,cor(sv,nv))
    }
    
  }
  return(ov)
}