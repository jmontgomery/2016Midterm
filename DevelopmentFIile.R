setwd("~/GitHub/2016Midterm")
#package.skeleton("integrateIt")

library(devtools)
library(roxygen2)
current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)



tryx<-seq(-5, 4.99, by=.95)
length(tryx)
tryy<-dnorm(tryx)

exampSimp<-integrateIt(tryx, tryy, -5, 5, rule="Simp")
exampTrap<-integrateIt(tryx, tryy, -5, 5, rule="Trap")
print(exampSimp)
plot(exampSimp)
print(exampTrap)
plot(exampTrap)


