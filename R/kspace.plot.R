kspace.plot<-function(filename,
                      center.correction=NA,
                      radius.correction=NA,
                      phi.correction=0,
                      lambda0=as.numeric(substr(filename,1,3)),
                      image.colour=NA,
                      ...
){
  
  require("jpeg")
  #require("fields")
  image.plot<-fields::image.plot
  #require("photonMonkey")
  
  rawImage<-readJPEG(source=filename,native=F)
  
  # a<-rawImage[,,1]+rawImage[,,2]+rawImage[,,3] #add all the channels together
  
  nx<-dim(rawImage)[1]  # number of x pixels
  ny<-dim(rawImage)[2]  # number of y pixels
  averageIntensity<-rawImage[,,1]+rawImage[,,2]+rawImage[,,3] #add all the channels together
  
  
  x<-rep(1:nx,ny)     # setup x coordinate matrix
  dim(x)<-c(nx,ny)    # set correct dimensions of matrix
  x<-x-nx/2           # wild guess that the center of the scattergram will be centeral to the image
  
  y<-rep(1:ny,nx)     # setup x coordinate matrix
  dim(y)<-c(ny,nx)    # set correct dimensions of matrix
  y<-y-ny/2           # wild guess that the center of the scattergram will be centeral to the image
  y<-t(y)             # transpose matrix
  
  z<-averageIntensity
  
  ### Center Correction ###
  if(is.na(center.correction[1])){
    
    center.coords<-c(0,0)
    par(bg='white',pty='s')
    image.plot(x,y,z,col=grey(1:100/100),pty='s')
    lines(rep(center.coords[1],2),c(-ny,ny),col=2,lwd=3)
    lines(c(-nx,nx),rep(center.coords[2],2),,col=2,lwd=3)
    title("CENTER CORRECTION")
    
    center.coords<-locator(1) # Get correction
    x<-x-center.coords$x  # Make correction
    y<-y-center.coords$y
  }else{
    center.coords<-center.correction
    x<-x-center.coords[1]
    y<-y-center.coords[2]
  }
  
  ### Radius Correction ###
  if(is.na(radius.correction[1])){
    par(bg='white',pty='s')
    image.plot(x,y,z,col=grey(1:100/100))
    title("RADIUS CORRECTION")
    
    radius.coords<-locator(1)
    z[which(sqrt((x^2+y^2))>=sqrt(radius.coords$x^2+radius.coords$y^2))]<-NA
  }else{
    radius.coords<-radius.correction
    z[which(sqrt((x^2+y^2))>=sqrt(radius.coords[1]^2+radius.coords[2]^2))]<-NA
  }
  
  
  ### Coordinate Transformations ###
  max.theta<-max(sqrt(x[which(!is.na(z))]^2+y[which(!is.na(z))]^2),na.rm=T)
  theta<-(pi/2)*(sqrt(x^2+y^2)/max.theta)
  phi<-atan2(y,x)
  
  k0<-2*pi/(lambda0*1e-9)
  r<-sin(theta)
  phi.correction.radians<-phi.correction*pi/180
  
  kx<-k0*r*cos(phi+phi.correction.radians)
  ky<-k0*r*sin(phi+phi.correction.radians)
  
  
  ### Make Plot ###
  par(bg='white',xaxs='i',yaxs='i', family="CMU Serif",mar=c(4,4.5,1,1))
  
  if(is.na(image.colour)){
    colScale<-colorRampPalette(c('black',rgb(max(rawImage[,,1]),max(rawImage[,,2]),max(rawImage[,,3]),1,maxColorValue=1)))
  }else{
    colScale<-colorRampPalette(c('black',image.colour))
  }
  
  image.plot(kx,ky,z/max(z,na.rm=T),
             col=colScale(100),
             xlim=range(kx,na.rm=T),
             ylim=range(ky,na.rm=T),
             #zlim=range(z,na.rm=T),
             #zlim=c(0,0.9),
             transparent.color=rgb(red=1,green=1,blue=1,alpha=0,maxColorValue=1),
             asp=1,xaxt='n',yaxt='n',
             xlab=expression(paste(italic(k[x])," (m"^"-1",")")),
             ylab=expression(paste(italic(k[y])," (m"^"-1",")")),
             ...
  )
  fancyRAxis(1)
  fancyRAxis(2)
  
  #fill the background black
  polygon.circle<-circle(x.center=0,0,k0)
  kx.range<-par("usr")[1:2]
  ky.range<-par("usr")[3:4]
  polygon(c(kx.range[2],polygon.circle[,1],kx.range[2],kx.range[2],kx.range[1],kx.range[1],kx.range[2]),c(0,polygon.circle[,2],0,ky.range[1],ky.range[1],ky.range[2],ky.range[2]),col='black')
  box()
  
  
  info<-list(center.correction=as.data.frame(center.coords),radius.correction=as.data.frame(radius.coords),phi.correction=as.data.frame(phi.correction),kx=kx,ky=ky,z=z)
  return(info)
}
