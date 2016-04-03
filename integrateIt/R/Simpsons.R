setClass(Class="Simpson",
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

setMethod("initialize", "Simpson", 
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
)
                    
setMethod("print", "Simpson",
                    function(x){
                      cat("Integration using Simpson's method \n")
                      cat(x@answer)
                    }
          )


setMethod("plot", "Simpson", 
          function(x, y=NULL){
            
            ## code below is for plotting the parabolas
            obj<-x
            
            ## extract all of the numbers we need
            n<-length(obj@x)
            u<-obj@x[seq(1, (n-2), by=2)]
            fu<-obj@y[seq(1, (n-2), by=2)]
            v<-obj@x[seq(2, (n-1), by=2)]
            fv<-obj@y[seq(2, (n-1), by=2)]
            w<-obj@x[seq(3, (n), by=2)]
            fw<-obj@y[seq(3, (n), by=2)]
            
            
            ## write a subfunction to help keep all of the numbers straight
            parabFun<-function(x, index=1, u, v, w, fu, fv, fw){
              firstPart<-fu[index]*(x -v[index])*(x-w[index])/((u[index]-v[index])*(u[index]-w[index]))
              secondPart<-fv[index]*(x -u[index])*(x-w[index])/((v[index]-u[index])*(v[index]-w[index]))
              thirdPart<- fw[index]*(x -u[index])*(x-v[index])/((w[index]-u[index])*(w[index]-v[index]))
              return(firstPart+secondPart+thirdPart)
            }
            
            
            ## Make the plot
            plot(obj@x, obj@y, pch=19, col="red")
            segments(obj@x, obj@y, obj@x, 0)
            
            for(i in 1:length(u)){
              plotX<-seq(u[i], w[i],length=100)
              plotY<-parabFun(plotX,index=i, u=u, v=v, w=w, fu=fu, fv=fv,fw=fw)
              lines(plotX, plotY)
            }
            
          }
)          