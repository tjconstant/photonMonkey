circle<-function(x.center,y.center,radius,...){
  
  
  nseg=360
  
  
  xx <- x.center + radius*cos( seq(0,2*pi, length.out=nseg) )
  yy <- y.center + radius*sin( seq(0,2*pi, length.out=nseg) )
  
  print(x.center)
  
  #lines(xx,yy,...)
  
  return(cbind(xx,yy))
}