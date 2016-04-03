integrateIt<-function(x, y, a, b, rule="Trap"){
  ## Do the trapezoidal
  n<-length(x) # number of points
  if(rule=="Trap"){
    h<-(b-a)/(2*n) # calculate h. Put 2 in the denominator
    answer<-h*(2*sum(y[2:(n-1)])+y[1]+y[n]) # calculate the outcome
    out<-new("Trapezoid", x=x, y=y, answer=answer)
  }
  else {
    h<-(b-a)/(n*3) ## calculate h.  Put 3 in the denominator
    first<-y[1] ## first element
    last<-y[n] ## second element
    evens.sum<-sum(y[seq(2, n-1, by=2)]) ## every other elements excludign first and last
    odds.sum<-sum(y[seq(3, n-2, by=2)]) # the rest
    answer<-h*(first+4*evens.sum + 2*odds.sum + last) #calculate output
    out<-new("Simpson", x=x, y=y, answer=answer)
  }
}
