generate_DFG_maps<-function(lambda_1=seq(545,615,0.5),lambda_2=seq(545,615,0.5),theta_1=20,theta_2=40,filters=seq(350,850,50),suppress_wavelength_plot=F){
  
  lambda_1_m<-matrix(data = lambda_1,nrow = length(lambda_1),ncol=length(lambda_1))
  lambda_2_m<-matrix(data = lambda_2,nrow = length(lambda_2),ncol=length(lambda_2))
  
  freq_4wm<-(2*3e8/(lambda_1_m*1e-9)-1*3e8/t(lambda_2_m*1e-9))*1e-12
  
  freq_dfg<-(3e8/(lambda_1_m*1e-9)-3e8/t(lambda_2_m*1e-9))*1e-12
  
  
  image.plot<-fields::image.plot
  
  if(suppress_wavelength_plot==F){
    image.plot(lambda_1,lambda_2,freq_4wm,nlevel = 128,
               xlab=bquote(lambda[1]~(nm)),
               ylab=bquote(lambda[2]~(nm)),
               legend.lab=expression(f[4*W*M]))
    
    if(!is.na(filters[1])){
      contour(lambda_1,lambda_2,freq_4wm,levels = 1e-12*3e8/(filters*1e-9), labels = paste(filters,"nm"),add=T)
    }
    
   image.plot(lambda_1,lambda_2,abs(freq_dfg),nlevel = 128,
               xlab=bquote(lambda[1]~(nm)),
               ylab=bquote(lambda[2]~(nm)),
               legend.lab=expression(f[DFG]))
    
    if(!is.na(filters[1])){
      contour(lambda_1,lambda_2,freq_4wm,levels = 1e-12*3e8/(filters*1e-9), labels = paste(filters,"nm"),add=T)
    }
  }
  
}
