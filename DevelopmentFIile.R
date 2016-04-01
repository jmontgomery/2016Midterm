


integrateIt<-function(x, y, a, b, rule="Trap"){
  ## Do the trapezoidal
  n<-length(x) # number of points
  if(rule=="Trap"){
    h<-(b-a)/(2*n) # calculate h. Put 2 in the denominator
    out<-h*(2*sum(y[2:(n-1)])+y[1]+y[n]) # calculate the outcome
    }
  if(rule=="Simp"){
    h<-(b-a)/(n*3) ## calculate h.  Put 3 in the denominator
    first<-y[1] ## first element
    last<-y[n] ## second element
    evens.sum<-sum(y[seq(2, n-1, by=2)]) ## every other elements excludign first and last
    odds.sum<-sum(y[seq(3, n-2, by=2)]) # the rest
    out<-h*(first+4*evens.sum + 2*odds.sum + last) #calculate output
  }
  return(out)  
}

tryx<-seq(-5, 4.99, by=.01)
length(tryx)
tryy<-dnorm(tryx)

integrateIt(tryx, tryy, -5, 5, rule="Simp")
