ellipse<-function(x,y,a,b){
  
  
  nseg=360
  x.cent <- x
  y.cent <- y
  
  xx <- x.cent + a*cos( seq(0,2*pi, length.out=nseg) )
  yy <- y.cent + b*sin( seq(0,2*pi, length.out=nseg) )
  
  lines(xx,yy, col='red')
  
  return(cbind(xx,yy))
  
}
