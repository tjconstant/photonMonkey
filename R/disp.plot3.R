disp.plot3<-function(x,                     # x vector
                     y,                     # y vector
                     z,                     # z vector
                     fx=x,                  # functional transformation of length(x)
                     fy=y,                  # functional transformation of length(y)
                     nx=length(unique(x)),  #length of unique x vector values 
                     ny=length(unique(y)),  #length of unique y vector values
                     ...){                  # graphical parameters
  
  require("fields")
  
  dim(fx)<-c(nx,ny)
  dim(fy)<-c(nx,ny)
  dim(z)<-c(nx,ny)
  
  if(1<2){
    image.plot(fx,fy,z,nlevel=100,...)
    
  }else{
    interpolated_Matrix<-akima:::interp(fx,fy,z,duplicate="mean",linear=FALSE,extrap=FALSE,xo=seq(min(fx), max(fx), length = 40),
                                        yo=seq(min(fy), max(fy), length = 40))
    image.plot(interpolated_Matrix$x,interpolated_Matrix$y,interpolated_Matrix$z,nlevel=100,col=grey(0:100/100),...)
  }
  
}

