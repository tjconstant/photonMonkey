semicircle<-function(x.center,y.center,radius,start_angle,end_angle,...){
  
  
  nseg=360
  
  start_angler=start_angle*pi/180
  end_angler=end_angle*pi/180
  
  xx <- x.center + radius*cos( seq(start_angler,end_angler, length.out=nseg) )
  yy <- y.center + radius*sin( seq(start_angler,end_angler, length.out=nseg) )
  
  print(x.center)
  
  #lines(xx,yy,...)
  
  return(cbind(xx,yy))
}