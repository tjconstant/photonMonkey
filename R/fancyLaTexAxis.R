fancyLaTexAxis<-function(side=1){
  
  text(0,1,"") # workaround for axis labels
  
  axLabels<-paste(axTicks(side)/10^log10(max(abs(axTicks(side)))),"$\\times 10^{",log10(max(abs(axTicks(side)))),"}$",sep="")
  axLabels[axTicks(side)==0]<-"0"
  
  axis(side, at = axTicks(side), label = axLabels)

}


fancyRAxis<-function(side=1){
  
  text(0,1,"") # workaround for axis labels
  require("sfsmisc")
  
  axLabels<-axTexpr(side=side)
  axLabels[axTicks(side)==0]<-"0"
  
  axis(side,at=axTicks(side),label=axLabels)
  
}