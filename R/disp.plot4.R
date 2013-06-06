data(SPPdispersion)
x<-SPPdispersion$wavelength*1e9 #needed to make scales for x and y similar for interp()
y<-SPPdispersion$angle
z<-SPPdispersion$reflection

#disp.plot3(x,y,z,(2*pi/(x*1e-9))*sin(y*pi/180),2*pi*3e8/(x*1e-9))

smooth_value<-400

library(akima)
int_spp<-interp(x,y,z,xo=seq(min(x),max(x),,smooth_value),yo=seq(min(y),max(y),,smooth_value))

x2<-matrix(int_spp$x,nrow=smooth_value,ncol=smooth_value)
y2<-matrix(int_spp$y,nrow=smooth_value,ncol=smooth_value,byrow=T)
z2<-int_spp$z

image.plot((2*pi/(x2*1e-9))*sin(y2*pi/180),2*pi*3e8/(x2*1e-9),z2,xaxs='i',yaxs='i',col=grey(0:100/100))
#add.diffraction(kgx=2*pi/600e-9,kgv=2*pi/150e-9)