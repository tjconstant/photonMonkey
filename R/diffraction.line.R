diffraction.line<-function(k,kgx,kgv,m,n,phi=0,alpha=pi/2){
  
  return(data.frame(
    k=k,
    omega=3e8*(sqrt(((k*cos(phi)+m*kgx-n*kgv*cos(alpha))^2)+((k*sin(phi)-n*kgv*sin(alpha))^2)))
    ))
  
}