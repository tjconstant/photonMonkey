disp.plot <- function(tk,tf,tr,plotXLim=c(min(tk,na.rm=TRUE),max(tk,na.rm=TRUE)),plotYLim=c((min(tf,na.rm=TRUE)),max(tf,na.rm=TRUE)),ncol=64,nrow=64,smooth=0,smoothY=smooth,zScale=c(0,1),colScale=grey(0:1000/1000),...){

  .Defunct(msg = "disp.plot is now defunct.\n Please see http://github.com/tjconstant/disp.plot for the latest version.")
  
  
warning("disp.plot() is depreciated and will no longer be supported in future versions of photonMonkey.\n Use instead disp.plot6()")
  
#require("fields",quietly=T)
require("sfsmisc",quietly=T)

as.image<-fields::as.image
make.surface.grid<-fields::make.surface.grid
interp.surface<-fields::interp.surface
image.plot<-fields::image.plot
as.surface<-fields::as.surface



# zscale setup
#zScale<-c()
#if (forceZeroOne == TRUE) zScale<-c(0,1) else zScale<-c(min(tr,na.rm=TRUE),max(tr,na.rm=TRUE))

imageZ<-as.image(tr, cbind(tk,tf), nrow = nrow, ncol=ncol, na.rm = TRUE)


if (smooth > 0) {

	tempX<-seq(min(tk,na.rm=TRUE),max(tk,na.rm=TRUE),,smooth) #sampling x tables for interpolation
	tempY<-seq(min(tf,na.rm=TRUE),max(tf,na.rm=TRUE),,smoothY) #sampling y tables for interpolation
	obj<-as.list(imageZ)
	make.surface.grid( list( tempX,tempY))-> loc	#interpolating grid
	interp.surface( obj, loc)-> look			#interpolation of the surface


	#image(as.surface(loc,look),ylim=plotYLim,xlim=plotXLim,zlim=zScale,col=colScale,...) #PLOT!
	image.plot(as.surface(loc,look),ylim=plotYLim,xlim=plotXLim,zlim=zScale,col=colScale,xaxt="n",yaxt="n",...)
	#image.plot(as.surface(loc,look),legend.only=TRUE,...)

} else {
	#mage(imageZ,ylim=plotYLim,xlim=plotXLim,zlim=zScale,col=colScale,axes=FALSE,...) #PLOT!
	image.plot(imageZ,ylim=plotYLim,xlim=plotXLim,zlim=zScale,col=colScale,xaxt="n",yaxt="n",...)
	#image.plot(as.surface(loc,look),legend.only=TRUE,...)

}

lines(c(0,0),c(0,0)) #really fucking weird workaround for decent labels
aX <- axTicks(1)
aY <- axTicks(2)

axis(1, at = aX, label = axTexpr(1, aX, drop.1=FALSE))
axis(2, at = aY, label = axTexpr(2, aY, drop.1=FALSE))

box()

if (smooth > 0) return(as.surface(loc,look)) else return(imageZ)

}

