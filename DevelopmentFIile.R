


integrateIt<-function(x, y, a, b, rule="Trap"){
  if(rule=="Trap"){
    n<-length(x)
    h<-(b-a)/(2*n)
    out<-h*(2*sum(x[2:(n-1)])+x[1]+x[n])
  }  
  return(out)  
}

x<-seq(-5, 5, by=.01)

y<-dnorm(x)

integrateIt(x, y, -5, 5, rule="Trap")
