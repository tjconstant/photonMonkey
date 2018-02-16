disp.plot4<-function(x,y,z,fx=nothing_x,fy=nothing_y,nx=length(unique(x)),ny=length(unique(y)),...){

  .Defunct(msg = "disp.plot4 is now defunct.\n Please see http://github.com/tjconstant/disp.plot for the latest version.")
  
  warning("disp.plot4() is depreciated and will no longer be supported in future versions of photonMonkey.\n Use instead disp.plot6()")
  
  interp<-akima::interp
  int_spp<-interp(x,y,z,xo=seq(min(x),max(x),length=nx),yo=seq(min(y),max(y),length=ny),linear=T)
  
  x2<-matrix(int_spp$x,nrow=nx,ncol=ny)
  y2<-matrix(int_spp$y,nrow=nx,ncol=ny,byrow=T)
  z2<-int_spp$z
  
  image.plot<-fields::image.plot
  image.plot(fx(x2,y2),fy(x2,y2),z2,xaxs='i',yaxs='i',...)

}

nothing_x<-function(x,y) return(x)
nothing_y<-function(x,y) return(y)