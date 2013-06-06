add.diffraction<-function(
  k=seq(par()$usr[1],par()$usr[2],,100), #the k range to use. default is the current plotting coordinates
  kgx,                                   #in-plane (phi=0) grating vector
  kgv,                                   #out-of-plane (phi=alpha) grating vector
  m.range=c(-5,5),                       #range of m*kgx values to evaluate
  n.range=c(-5,5),                       #range of n*kgv values to evaluate
  phi = 0,                               #azimuthal angle in radians
  alpha = pi/2,                          #angle between grating vectors in radians
  ...){

  
for(n in -5:5){
  for(m in -5:5){
    
      in.plane<-(sqrt(((k * cos(phi) + m * kgx - n * kgv * cos(alpha))^2) + ((k * sin(phi) -  n * kgv * sin(alpha))^2)))
    
      #if(any(max(k)^2>(in.plane)^2)){
      
        lines(diffraction.line(k, kgx, kgv, m, n, phi, alpha),...)
      #}

    }
  }
}

