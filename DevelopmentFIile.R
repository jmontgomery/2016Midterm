setwd("~/GitHub/2016Midterm")
#package.skeleton("integrateIt")

rm(list=ls())

library(devtools)
library(roxygen2)
current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)


# an example
tryx<-seq(-5, 4.99, by=.95)
length(tryx)
tryy<-dnorm(tryx)

exampSimp<-integrateIt(tryx, tryy, -5, 5, rule="Simp")
exampTrap<-integrateIt(tryx, tryy, -5, 5, rule="Trap")
print(exampSimp)
plot(exampSimp)
print(exampTrap)
plot(exampTrap)

help(integrateIt)
help(Trapezoid)
help(Simpson)

# testing the validity checks
tryx<-seq(-5, 4.99, by=.9)
length(tryx)


exampSimp<-integrateIt(tryx, tryy, -5, 5, rule="Simp")
exampTrap<-integrateIt(tryx, tryy, -5, 5, rule="Trap")

tryy[1]<-NA
exampTrap<-integrateIt(tryx, tryy, -5, 5, rule="Trap")

tryy<-dnorm(tryx)
tryy<-tryy[-1]
exampTrap<-integrateIt(tryx, tryy, -5, 5, rule="Trap")


