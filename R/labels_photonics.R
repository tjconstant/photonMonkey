label_in_plane_wavevector<-function(subscript="x",si_prefix="phantom()"){
parse(text=
          paste("list('in'-plane~momentum,~italic(k)[",subscript,"]~(",si_prefix,"*m^-1))",sep="")
        )
}

label_angular_frequency<-function(){
  expression(paste("angular frequency, ", 
                   omega, " (rad ", s^-1, ")"))
}

label_polar_angle<-function(){
  expression(paste("polar angle, ",theta, " (",degree,")"))
}

label_azimuthal_angle<-function(){
  expression(paste("polar angle, ",phi, " (",degree,")"))
}

label_reflection<-function(subscript="pp"){
  parse(text=paste("italic(R)[",subscript,"]",sep=""))
}

label_wavelength<-function(si_prefix="phantom()"){
  return(parse(text=paste("list(wavelength,~(",si_prefix,"*m))",sep="")))
}

label_frequency<-function(si_prefix="phantom()"){
  return(parse(text=paste("list(frequency,~(",si_prefix,"*Hz))",sep="")))
}

plot(NA,NA,xlim=c(0,1),ylim=c(0,1))
text(0.5,1,label_angular_frequency())
text(0.5,0.9,label_azimuthal_angle())
text(0.5,0.8,label_in_plane_wavevector(subscript="gamma",si_prefix="c"))
text(0.5,0.7,label_polar_angle())
text(0.5,0.6,label_reflection("TM"))
text(0.5,0.5,label_wavelength(si_prefix="mu"))
text(0.5,0.4,label_frequency(si_prefix="G"))
lines(0,0)