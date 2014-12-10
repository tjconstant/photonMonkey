read.delay_scan<-function(filename,
                          time_max=1,
                          quick_plot=T){
  
  
  data<-read.table(filename)$V1
  time<-seq(0,time_max,,length(read.table(filename)$V1))
  
  if(quick_plot){
    plot(time,data,xlab="time (ps)",pch=16);grid();lines(time,data)
  }
  
  return(data.frame(time,data))
  
}