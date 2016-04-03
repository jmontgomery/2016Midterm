setwd("~/GitHub/2016Midterm")
#package.skeleton("integrateIt")





tryx<-seq(-5, 4.99, by=.95)
length(tryx)
tryy<-dnorm(tryx)

exampSimp<-integrateIt(tryx, tryy, -5, 5, rule="Simp")
exampTrap<-integrateIt(tryx, tryy, -5, 5, rule="Trap")
print(exampSimp)
plot(exampSimp)
print(exampTrap)
plot(exampTrap)


