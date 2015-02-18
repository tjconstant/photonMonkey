generate_4WM_maps<-function(lambda_1=seq(545,615,0.5),lambda_2=seq(545,615,0.5),theta_1=20,theta_2=40,filters=seq(350,850,50),suppress_wavelength_plot=F){
  
  lambda_1_m<-matrix(data = lambda_1,nrow = length(lambda_1),ncol=length(lambda_1))
  lambda_2_m<-matrix(data = lambda_2,nrow = length(lambda_2),ncol=length(lambda_2))
  
  lambda_4wm<-(2/lambda_1_m-1/t(lambda_2_m))^-1
  
  lambda_dfg<-(1/lambda_1_m-1/t(lambda_2_m))^-1
  
  
  image.plot<-fields::image.plot
  
  if(suppress_wavelength_plot==F){
    image.plot(lambda_1,lambda_2,lambda_4wm,nlevel = 128,
               xlab=bquote(lambda[1]~(nm)),
               ylab=bquote(lambda[2]~(nm)),
               legend.lab=expression(lambda[4*W*M]))
    
#   image.plot(lambda_1,lambda_2,lambda_dfg,nlevel = 128,
#               xlab=bquote(lambda[1]~(nm)),
#               ylab=bquote(lambda[2]~(nm)),
#               legend.lab=expression(lambda[DFG]))
    
    if(!is.na(filters[1])){
      contour(lambda_1,lambda_2,lambda_4wm,levels = filters, labels = paste(filters,"nm"),add=T)
    }
  }
  

  angle_4wm<-180*asin((lambda_4wm)*((2*sin(theta_1*pi/180)/lambda_1_m)-(sin(theta_2*pi/180)/t(lambda_2_m))))/pi
  
  image.plot(lambda_1,lambda_2,angle_4wm,nlevel = 128,
             xlab=bquote(lambda[1]~(nm)~~theta[1]==.(theta_1)*degree),
             ylab=bquote(lambda[2]~(nm)~~theta[2]==.(theta_2)*degree),
             legend.lab=expression(theta[4*W*M]))
  if(!is.na(filters[1])){
    contour(lambda_1,lambda_2,lambda_4wm,levels = filters, labels = paste(filters,"nm"),add=T)
  }
  
}
