permittivity.PalikAg<-function(wavelength){
  
  # E. D. Palik, Handbook of Optical Constants of Solids (Academic Press, 1985), p. 1096.
  
  data(PalikAg)
  epsilon.real <- splinefun(PalikAg$Wavelength,PalikAg$EpsilonReal)
  epsilon.imag <- splinefun(PalikAg$Wavelength,PalikAg$EpsilonImaginary)
  
  if(wavelength > 2000 || wavelength < 200) warning("wavelength out of dataset range")
  
  return(data.frame(wavelength=wavelength,epsilon.real=epsilon.real(wavelength),epsilon.imaginary=epsilon.imag(wavelength)))
}