mirror<-function(m){
  x<-m$x
  y<-m$y
  z<-m$z
  
  md<-rbind(cbind(x,y,z),cbind(-x[x>0],y[x>0],z[x>0]))
  
  mdd<-as.data.frame(md)
  
  return(mdd[with(mdd,order(y,x)),])
}