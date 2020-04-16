impCalc<-function(){
  imp<-data.frame()
  for(i in 3:32){
    temp<-t.test(wbcd[,i]~wbcd$diagnosis)
    imp<-rbind(imp,cbind(i,temp$p.value))
  }
  return(imp)
}

imp<-impCalc()
View(imp)