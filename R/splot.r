splot<-function(data,ref,dkf){
  
  warning("splot() is depreciated and will no longer be supported in future versions of photonMonkey.\n Use instead spectrum.normalise()")
  
  #dark field correction

  d<-data[,3]-mean(dkf[,3])
  r<-ref[,3]-mean(dkf[,3])

  dr<-data[,4]-mean(dkf[,4])
  rr<-ref[,4]-mean(dkf[,4])

  normData<-(d/r)
  normRef<-(dr/rr)

  par(mfrow=c(2,2))

#   plot(data[,1],d, type='l', ylim=c(min(c(d,r,dr,rr)),max(c(d,r,dr,rr))), main="unnormalised data", xlab="wavelength (nm)", ylab="R")
#   lines(ref[,1],r, type='l', col='red')
#   lines(data[,1],dr, type='l', col='blue')
#   lines(ref[,1],rr, type='l', col='green')

#   legend(max(data[,1]/2), max(c(d,r,dr,rr))/3, c("data","reference spectra", "data reference detector", "reference referenece detector"), lty=1, col=c("black","red","blue","green"))
#   grid()
# 
# 
#   plot(data[,1],normData/normRef, type='l', ylim=c(min(c(normData,normRef)),max(c(normData,normRef))), col='blue', main="normalised data and noise", xlab="wavelength (nm)", ylab="R")
#   lines(data[,1],normRef, type='l', col='red')
#   lines(data[,1],normData, type='l', col='black')
#   grid()

#   plot(data[,1],normData, type='l', main="data normalised to ref spectra", xlab="wavelength (nm)", ylab="R")
#   grid()
# 
#   plot(data[,1],normData/normRef, type='l', ylim=c(0,1), main="data fully normalised", xlab="wavelength (nm)", ylab="R")
#   grid()

#  par(mfrow=c(1,1))
  return(normData/normRef)
}