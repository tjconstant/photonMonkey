label_scientific10 <- function(x) {
  
  a<-gsub("e", " %*% 10^", scientific_format()(x))
  b<-gsub("+","",a)
  b[x==0]<-"0"
  return(parse(text=b))
}
