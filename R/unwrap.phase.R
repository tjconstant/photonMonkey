#homemade unwrap function
unwrap.phase<-function(Data,tol=pi,step=(2*pi)){
  
  #step indicates the amount size of the jump in the phase data inputted
  #tol is the amount between steps which is considered large enough to be a jump
 
  Data_Length<-length(Data)
  
  for(a in 1:(Data_Length-1)){
    
    b<-a+1
    Data_Difference<-Data[a]-Data[b]
    if(Data_Difference<=(-tol)){
      for(c in b:Data_Length){
        Data[c]<-Data[c]-step
      }
    }
    if(Data_Difference>=(tol)){
      for(c in b:Data_Length){
        Data[c]<-Data[c]+step
      }
    }  
  }
  return(Data)
}