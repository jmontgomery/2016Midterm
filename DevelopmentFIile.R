


integrateIt<-function(x, y, a, b, rule="Trap"){
  ## Do the trapezoidal
  n<-length(x) # number of points
  if(rule=="Trap"){
    h<-(b-a)/(2*n) # calculate h. Put 2 in the denominator
    answer<-h*(2*sum(y[2:(n-1)])+y[1]+y[n]) # calculate the outcome
    }
  if(rule=="Simp"){
    h<-(b-a)/(n*3) ## calculate h.  Put 3 in the denominator
    first<-y[1] ## first element
    last<-y[n] ## second element
    evens.sum<-sum(y[seq(2, n-1, by=2)]) ## every other elements excludign first and last
    odds.sum<-sum(y[seq(3, n-2, by=2)]) # the rest
    answer<-h*(first+4*evens.sum + 2*odds.sum + last) #calculate output
  }
  out<-list(x=x, y=y, answer=answer)
  return(out)  
}

tryx<-seq(-5, 4.99, by=.95)
length(tryx)
tryy<-dnorm(tryx)

exampSimp<-integrateIt(tryx, tryy, -5, 5, rule="Simp")
exampTrap<-integrateIt(tryx, tryy, -5, 5, rule="Trap")

## everything we need to plot the trapezoids
obj<-exampTrap
plot(obj$x, obj$y, type="l")
segments(obj$x, obj$y, obj$x, 0)


## code below is for plotting the parabolas
obj<-exampSimp

## extract all of the numbers we need
n<-length(obj$x)
u<-obj$x[seq(1, (n-2), by=2)]
fu<-obj$y[seq(1, (n-2), by=2)]
v<-obj$x[seq(2, (n-1), by=2)]
fv<-obj$y[seq(2, (n-1), by=2)]
w<-obj$x[seq(3, (n), by=2)]
fw<-obj$y[seq(3, (n), by=2)]


## write a subfunction to help keep all of the numbers straight
parabFun<-function(x, index=1, u, v, w, fu, fv, fw){
  firstPart<-fu[index]*(x -v[index])*(x-w[index])/((u[index]-v[index])*(u[index]-w[index]))
  secondPart<-fv[index]*(x -u[index])*(x-w[index])/((v[index]-u[index])*(v[index]-w[index]))
  thirdPart<- fw[index]*(x -u[index])*(x-v[index])/((w[index]-u[index])*(w[index]-v[index]))
  return(firstPart+secondPart+thirdPart)
}


## Make the plot
plot(obj$x, obj$y, pch=19, col="red")
segments(obj$x, obj$y, obj$x, 0)

for(i in 1:length(u)){
    plotX<-seq(u[i], w[i],length=100)
    plotY<-parabFun(plotX,index=i, u=u, v=v, w=w, fu=fu, fv=fv,fw=fw)
    lines(plotX, plotY)
}


