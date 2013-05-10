rcp.plot<-function(a=600e-9,b=400e-9,k0=2*pi/633e-9,alpha=90){
  require("sfsmisc")
  
  k.spp<-function(k0)
  {
    wp=1.32e16
    epsi.metal<-1-((3e8/k0)^2)/wp^2
    
    return(sqrt(epsi.metal/(1+epsi.metal)))
    
  }
  
  n=1;
  
  a<-a
  b<-b
  
  k0<-k0
  
  ra=2*pi/a;
  rb=2*pi/b;
  ralpha=180-alpha;
  ralpha<-ralpha*pi/180
  lim=max(c(ra,rb))

  plot(NA,NA,xlim=c(-lim*n-1.5e6,lim*n+1.5e6),ylim=c(-lim*n-1.5e6,lim*n+1.5e6),xlab=expression(k[x]),ylab=expression(k[y]),xaxt="n",yaxt="n",asp=1)
  lines(c(0,0),c(0,0)) #really fucking weird workaround for decent labels
  aX <- axTicks(1)
  aY <- axTicks(2)

  axis(1, at = aX, label = axTexpr(1, aX, drop.1=FALSE))
  axis(2, at = aY, label = axTexpr(2, aY, drop.1=FALSE))
  
  arrows(0,0,1*ra+0*rb*cos(ralpha),0*rb*sin(ralpha),col='red',lwd=2)
  arrows(0,0,0*ra+1*rb*cos(ralpha),1*rb*sin(ralpha),col='red',lwd=2)
  lines(semicircle(x.center=0,y.center=0,radius=3e6,start_angle=0,end_angle=ralpha),col=2,lwd=2,lty=1)
  
  k<-c()
  
  m<-5*n
  for(i in -m:m){
    for( j in -m:m){
      points(i*ra+j*rb*cos(ralpha),j*rb*sin(ralpha),pch=16)
      #text(i*ra+j*rb*cos(ralpha),j*rb*sin(ralpha)-1.5e6,paste(i,",",j))
      
      k0_circle<-circle(x.center=i*ra+j*rb*cos(ralpha),y.center=j*rb*sin(ralpha),radius=k0)
      lines(k0_circle,lty=1,col=4,lwd=3)
      
      lines(circle(i*ra+j*rb*cos(ralpha),j*rb*sin(ralpha),radius=k0*1.1),col=1,lty=3,lwd=1)
      
      
    }
  }
  
  lines(circle(0,0,k0,),lty=1,lwd=3)
  
  
  print(paste("lambda0 = ",signif(2*pi/k0,3)))
  
  #x.bz<-seq(-k0,k0,,100)
  #y.bz<-seq(-k0,k0,,100)
  
  #m<--1
  #n<-0
  #lines(-tan(75*pi/180)*x.bz+m*cos(75*pi/180)*(pi/b)+n*(pi/a),y.bz+m*sin(75*pi/180)*pi/b,lwd=4,xlim=c(0,k0/8))
  
  #m<-1
  #n<-0
  #lines(-tan(75*pi/180)*x.bz+m*cos(75*pi/180)*(pi/b)+n*(pi/a),y.bz+m*sin(75*pi/180)*pi/b,lwd=4,xlim=c(0,k0))
  
  #lines(rep(pi/a,100),y.bz,lwd=4)
  #lines(-rep(pi/a,100),y.bz,lwd=4)
  
  #m<-1
  #n<--1
  #lines((b/a)*(tan(75*pi/180))*x.bz+m*cos(75*pi/180)*(pi/b)+n*(pi/a),y.bz+m*sin(75*pi/180)*pi/b,lwd=4,xlim=c(0,k0))
  
  
  #m<--1
  #n<-1
  #lines((b/a)*(tan(75*pi/180))*x.bz+m*cos(75*pi/180)*(pi/b)+n*(pi/a),y.bz+m*sin(75*pi/180)*pi/b,lwd=4,xlim=c(0,k0))
  
  
  
  
}


