.onLoad<-function(libname=find.package("photonMonkey"),pkgname="photonMonkey"){
  
  cat("\nWelcome to photonMonkey! \nUtility functions for common tasks in electromagnetics.")
  

  wisdom<-c("MATLAB is for engineers and Pandas.",
            "Asking questions in group meetings won't make your dick any bigger.",
            "That thesis isn't going to write itself.",
            "Reloading photonMonkey to get new wisdom is not productive.",
            "The rooster is a ninja. Beware.",
            paste(weekdays(Sys.Date()),"Club!"),
            "A tower run under 40 seconds is an achievement. Under 35 seconds is a wasted life.",
            "The cleaner may knife you."
            )
  
  wisdom_index<-sample(1:length(wisdom),1,replace=T)
  
  cat(paste("\nA little wisdom:",wisdom[wisdom_index]))
}