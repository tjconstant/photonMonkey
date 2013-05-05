ggdisp.plot<-function(x,                     # x vector
                      y=NA,                     # y vector
                      z=NA,                     # z vector
                      fx=x,                  # functional transformation of length(x)
                      fy=y,                  # functional transformation of length(y)
                      midpoint=FALSE,
                      col=c("black","white")){
  
  # Function to plot non-rectangular shaped polygons for ireggular spaced data in ggplot2.
  # Adapted from field package poly.image() function to return a ggplot2 object.
  
  #if(any(is.na(y))){     work in progress
  #  warning("Treating x as complete dataset")
    
  #  y<-x[,2]
  #  z<-x[,3]
  #  x<-x[,1]
  #}
  
  poly.image.regrid<-fields::poly.image.regrid
  
  nx = length(unique(x))
  ny = length(unique(y))
  
  dim(fx) <- c(nx, ny)
  dim(fy) <- c(nx, ny)
  dim(z) <- c(nx, ny)
  
  x<-fx
  y<-fy
  
  Dx <- dim(fx)
  Dy <- dim(fy)
  if (any((Dx - Dy) != 0)) {
    stop(" x and y matrices should have same dimensions")
  }
  Dz <- dim(z)
  if (all((Dx - Dz) == 0) & !midpoint) {
    x <- poly.image.regrid(x)
    y <- poly.image.regrid(y)
  }
  
  N <- ncol(x)
  Nm1 <- N - 1
  M <- nrow(x)
  Mm1 <- M - 1
  
  xps<-c()
  yps<-c()
  zps<-c()
  ids<-c()
  
  for (i in (1:Mm1)) {
    xp <- cbind(x[i, 1:Nm1], x[i + 1, 1:Nm1], x[i + 1, 2:N], 
                x[i, 2:N], rep(NA, Nm1))
    yp <- cbind(y[i, 1:Nm1], y[i + 1, 1:Nm1], y[i + 1, 2:N], 
                y[i, 2:N], rep(NA, Nm1))
    
    id<-i*(length(rep(rep(1:Nm1),each=5))+1)+rep(rep(1:Nm1),each=5)
    xp <- c(t(xp))
    yp <- c(t(yp))
    
    xps<-c(xps,xp)
    yps<-c(yps,yp)
    ids<-c(ids,id)
    
    pcol <- c(z[i, 1:Nm1])
    zp<-rep(pcol,each=5)
    zps<-c(zps,zp)
    
  }
  
  ggdataframe<-data.frame(ids,xps,yps,zps)
  
  p <- ggplot(ggdataframe, aes(x=xps, y=yps))+
    geom_polygon(aes(fill=zps,color=zps,group=ids))+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))+
    scale_color_gradientn(colours=col,name="R")+
    scale_fill_gradientn(colours=col,name="R")+
    theme_bw()+
    xlab(expression(paste("in-plane momentum, ",italic(k[x])," (",m^-1,")")))+
    ylab(expression(paste("angular momentum, ", omega," (rad ",s^-1,")")))
  
  return(p)
}

