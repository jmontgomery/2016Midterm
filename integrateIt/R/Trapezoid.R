setClass(Class="Trapezoid",
         representation=representation(
           x="numeric",
           y="numeric",
           answer="numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
           answer=c()
         ))

setMethod("initialize", "Trapezoid", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 



setMethod("print", "Trapezoid",
          function(x){
            cat("Integration using trapezoidal method \n")
            cat(x@answer)
          }
)



## everything we need to plot the trapezoids
setMethod("plot", "Trapezoid", 
          function(x, y=NULL){
            obj<-x
            plot(obj@x, obj@y, type="l")
            segments(obj@x, obj@y, obj@x, 0)
          }
)

