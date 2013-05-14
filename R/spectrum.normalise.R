spectrum.normalise<-function(data,ref,dkf){
  
  #dark field correction
  
  d<-data[,3]-mean(dkf[,3])
  r<-ref[,3]-mean(dkf[,3])
  
  dr<-data[,4]-mean(dkf[,4])
  rr<-ref[,4]-mean(dkf[,4])
  
  normData<-(d/r)
  normRef<-(dr/rr)
  
  return(normData/normRef)
}