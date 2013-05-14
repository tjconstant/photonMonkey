permittivity.NashSamblesAg<-function(wavelength){
  
  # D. J. Nash and J. R. Sambles, J. Mod. Op. 43, 81–91 (1996).
  
  data(NashSamblesAg)
  epsilon.real <- splinefun(NashSamblesAg$Wavelength,NashSamblesAg$EpsilonReal)
  epsilon.imag <- splinefun(NashSamblesAg$Wavelength,NashSamblesAg$EpsilonImaginary)
  
  if(wavelength > 900 || wavelength < 450) warning("wavelength out of dataset range")
  
  return(data.frame(wavelength=wavelength,epsilon.real=epsilon.real(wavelength),epsilon.imaginary=epsilon.imag(wavelength)))
}